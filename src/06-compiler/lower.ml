open Emit
module Types = Wasm.Types
module Ast = Language.Ast
module Source = Utils.Source
module Const = Language.Const
module Env = Env

let box_locals = ref false
let box_globals = ref true
let box_modules = ref true
let box_temps = ref false
let box_scrut = ref false

type null = Null | Nonull

type loc =
  | LocalLoc of int
  | GlobalLoc of int
  | ClosureLoc of null * int * int * int (* fldidx, localidx, typeidx *)

type func_loc = { funcidx : int; typeidx : int; arity : int }

let as_local_loc = function LocalLoc idx -> idx | _ -> assert false
let as_global_loc = function GlobalLoc idx -> idx | _ -> assert false

(* Representations *)

type rep =
  | DropRep (* value never used *)
  | BlockRep of null (* like Boxed, but empty tuples are suppressed *)
  | BoxedRep of null (* concrete boxed representation *)
  | BoxedAbsRep of null (* abstract boxed representation *)
  | UnboxedRep of
      null (* representation with unboxed type or concrete ref types *)
  | UnboxedLaxRep of null (* like Unboxed, but Int may have junk high bit *)

let print_rep = function
  | DropRep -> print_endline "DropRep"
  | BlockRep _ -> print_endline "BlockRep"
  | BoxedRep _ -> print_endline "BoxedRep"
  | BoxedAbsRep _ -> print_endline "BoxedAbsRep"
  | UnboxedRep _ -> print_endline "UnboxedRep"
  | UnboxedLaxRep _ -> print_endline "UnboxedLaxRep"

let null_rep = function
  | BlockRep n | BoxedRep n | BoxedAbsRep n | UnboxedRep n | UnboxedLaxRep n ->
      n
  | DropRep -> assert false

(* Configurable *)
let boxed_if flag null = if !flag then BoxedRep null else UnboxedRep null
let local_rep () = boxed_if box_locals Null (* values stored in locals *)
let clos_rep () = boxed_if box_locals Nonull (* values stored in closures *)
let global_rep () = boxed_if box_globals Null (* values stored in globals *)
let struct_rep () = boxed_if box_modules Nonull (* values stored in structs *)
let tmp_rep () = boxed_if box_temps Null (* values stored in temps *)
let pat_rep () = boxed_if box_scrut Nonull (* values fed into patterns *)

(* Non-configurable *)
let ref_rep = BoxedAbsRep Null (* expecting a reference *)
let rigid_rep = UnboxedRep Nonull (* values produced or to be consumed *)
let lax_rep = UnboxedLaxRep Nonull (* lax ints produced or consumed *)
let field_rep = BoxedAbsRep Nonull (* values stored in fields *)
let arg_rep = BoxedAbsRep Nonull (* argument and result values *)
let unit_rep = BlockRep Nonull (* nothing on stack *)

let loc_rep = function
  | GlobalLoc _ -> global_rep ()
  | LocalLoc _ -> local_rep ()
  | ClosureLoc _ -> clos_rep ()

let max_func_arity = 4
let clos_arity_idx = 0
let clos_code_idx = 1
let clos_env_idx = 2 (* first environment entry *)

(* Environment *)

type data_con = { tag : int; typeidx : int }
type data = (Ast.label * data_con) list

type env =
  (loc * func_loc option, data, Primitives.Primitives.primitive) Env.env

type scope = PreScope | LocalScope | GlobalScope

let make_env () =
  let env = ref Env.empty in
  env

let scope_rep = function
  | PreScope -> rigid_rep
  | LocalScope -> local_rep ()
  | GlobalScope -> global_rep ()

(* Compilation context *)

module ClosKey = struct
  type t = Types.val_type list * Types.val_type list * Types.field_type list

  let compare = compare
end

module ClosMap = Map.Make (ClosKey)
module IdxMap = Map.Make (Int32)

type clos_idxs = { codeidx : int; closidx : int; envidx : int }

type ctxt_ext = {
  envs : (scope * env ref) list;
  clostypes : clos_idxs ClosMap.t ref;
  data : int ref;
}

type ctxt = ctxt_ext Emit.ctxt

let make_ext_ctxt () : ctxt_ext =
  {
    envs = [ (PreScope, make_env ()) ];
    clostypes = ref ClosMap.empty;
    data = ref (-1);
  }

let make_ctxt () : ctxt = Emit.make_ctxt (make_ext_ctxt ())

let enter_scope ctxt scope : ctxt =
  {
    ctxt with
    ext = { ctxt.ext with envs = (scope, ref Env.empty) :: ctxt.ext.envs };
  }

let current_scope ctxt : scope * env ref = List.hd ctxt.ext.envs

let rec find_typ_var ctxt y envs : data =
  match envs with
  | [] ->
      Ast.TyName.print y Format.str_formatter;
      Printf.printf "[find_typ_var `%s`]\n%!" (Format.flush_str_formatter ());
      assert false
  | (_, env) :: envs' -> (
      match Env.find_opt_typ y !env with
      | None -> find_typ_var ctxt y envs'
      | Some { it = data; _ } -> data)

(* Lowering types *)

let lower_ref null ht : Types.ref_type =
  match null with Null -> (Null, ht) | Nonull -> (NoNull, ht)

let abs = Types.EqHT
let absref = lower_ref Nonull abs
let sub xs st = Types.SubT (Types.NoFinal, List.map (fun x -> x) xs, st)
let field t = Types.FieldT (Types.Cons, Types.ValStorageT t)
let field_mut t = Types.FieldT (Types.Var, Types.ValStorageT t)
let ref_ x = Types.RefT (Types.NoNull, Types.VarHT (Types.StatX x))

let rec lower_value_type ctxt rep t : Types.val_type =
  match (t, rep) with
  | t, (BlockRep n | BoxedRep n) | (Ast.TyApply _ as t), BoxedAbsRep n ->
      RefT (lower_ref n (lower_heap_type ctxt t))
  | _, BoxedAbsRep n -> RefT (lower_ref n abs)
  | Ast.TyConst Const.BooleanTy, _ -> NumT I32T
  | Ast.TyConst Const.IntegerTy, _ -> NumT I32T
  | Ast.TyConst Const.FloatTy, _ -> NumT F64T
  | Ast.TyParam _, _ -> RefT (NoNull, EqHT)
  | t, (UnboxedRep n | UnboxedLaxRep n) ->
      RefT (lower_ref n (lower_heap_type ctxt t))
  | _, DropRep -> assert false

and lower_heap_type ctxt t : Types.heap_type =
  match t with
  | Ast.TyConst Const.BooleanTy | Ast.TyConst Const.IntegerTy -> I31HT
  | Ast.TyTuple [] -> EqHT
  | Ast.TyApply _ -> EqHT
  | t -> VarHT (Types.StatX (lower_var_type ctxt t))

and lower_anycon_type ctxt : int =
  emit_type ctxt
    (sub []
       (Types.DefStructT
          (Types.StructT
             [
               Types.FieldT
                 (Types.Cons, Types.ValStorageT (Types.NumT Types.I32T));
             ])))

and lower_sum_type ctxt t : int =
  let anycon = lower_anycon_type ctxt in
  match t with
  | None ->
      emit_type ctxt
        (sub
           [ Types.VarHT (Types.StatX anycon) ]
           (Types.DefStructT
              (Types.StructT (field (Types.NumT Types.I32T) :: []))))
  | Some x ->
      let vt = lower_value_type ctxt field_rep x in
      let ft = (fun x -> Types.FieldT (Types.Cons, Types.ValStorageT x)) vt in
      emit_type ctxt
        (sub
           [ Types.VarHT (Types.StatX anycon) ]
           (Types.DefStructT
              (Types.StructT [ field (Types.NumT Types.I32T); ft ])))

and lower_inline_type ctxt t : int =
  let vt = lower_value_type ctxt field_rep t in
  let ft = (fun x -> Types.FieldT (Types.Cons, Types.ValStorageT x)) vt in
  emit_type ctxt (sub [] (Types.DefStructT (Types.StructT (ft :: []))))

and lower_ty_apply_type ctxt ts : int =
  if ts = [] then -1
  else
    let anycon = lower_anycon_type ctxt in
    let vts = List.map (lower_value_type ctxt field_rep) ts in
    let fts = List.map field vts in
    emit_type ctxt
      (sub
         [ Types.VarHT (Types.StatX anycon) ]
         (Types.DefStructT
            (Types.StructT (field (Types.NumT Types.I32T) :: fts))))

and lower_var_type ctxt t =
  match t with
  | Ast.TyConst Const.FloatTy ->
      emit_type ctxt
        (sub []
           (Types.DefStructT (Types.StructT [ field (Types.NumT Types.F64T) ])))
  | Ast.TyTuple ts ->
      let ts = List.map (lower_value_type ctxt field_rep) ts in
      emit_type ctxt
        (sub [] (Types.DefStructT (Types.StructT (List.map field ts))))
  | Ast.TyArrow _ as x ->
      let num_args = Ast.arity x in
      snd (lower_func_type ctxt num_args)
  | Ast.TyApply _ ->
      Ast.print_ty (Ast.new_print_param ()) t Format.std_formatter;
      failwith "tyapply"
  | Ast.TyParam _ ->
      Ast.print_ty (Ast.new_print_param ()) t Format.std_formatter;
      failwith "typaram"
  | _ -> assert false

and lower_anyclos_type ctxt : int =
  emit_type ctxt
    (sub []
       (Types.DefStructT (Types.StructT [ field (Types.NumT Types.I32T) ])))

and lower_func_type ctxt arity : int * int =
  let func ts1 ts2 = Types.DefFuncT (Types.FuncT (ts1, ts2)) in
  let argts, _ = lower_param_types ctxt arity in
  let key = (argts, [ Types.RefT absref ], []) in
  match ClosMap.find_opt key !(ctxt.ext.clostypes) with
  | Some { codeidx; closidx; _ } -> (codeidx, closidx)
  | None ->
      let anyclos = lower_anyclos_type ctxt in
      let code, def_code = emit_type_deferred ctxt in
      let closdt =
        sub
          [ Types.VarHT (Types.StatX anyclos) ]
          (Types.DefStructT
             (Types.StructT
                [
                  field (Types.NumT Types.I32T);
                  field
                    (Types.RefT (Types.NoNull, Types.VarHT (Types.StatX code)));
                ]))
      in
      let clos = emit_type ctxt closdt in
      let codedt =
        sub []
          (func
             (Types.RefT (Types.NoNull, Types.VarHT (Types.StatX clos)) :: argts)
             [ Types.RefT absref ])
      in
      def_code codedt;
      let clos_idxs = { codeidx = code; closidx = clos; envidx = clos } in
      ctxt.ext.clostypes := ClosMap.add key clos_idxs !(ctxt.ext.clostypes);
      (code, clos)

and lower_clos_type ctxt arity flds : int * int * int =
  let argts, _ = lower_param_types ctxt arity in
  let key = (argts, [ Types.RefT absref ], flds) in
  match ClosMap.find_opt key !(ctxt.ext.clostypes) with
  | Some { codeidx; closidx; envidx } -> (codeidx, closidx, envidx)
  | None ->
      let code, clos = lower_func_type ctxt arity in
      let envdt =
        sub
          [ Types.VarHT (Types.StatX clos) ]
          (Types.DefStructT
             (Types.StructT
                (field (Types.NumT Types.I32T) :: field (ref_ code) :: flds)))
      in
      let clos_env = emit_type ctxt envdt in
      let clos_idxs = { codeidx = code; closidx = clos; envidx = clos_env } in
      ctxt.ext.clostypes := ClosMap.add key clos_idxs !(ctxt.ext.clostypes);
      (code, clos, clos_env)

and lower_param_types ctxt arity : Types.val_type list * int option =
  let field_mut t = Types.FieldT (Types.Var, Types.ValStorageT t) in
  let array ft = Types.DefArrayT (Types.ArrayT ft) in
  if arity <= max_func_arity then
    (List.init arity (fun _ -> Types.RefT absref), None)
  else
    let argv =
      emit_type ctxt (sub [] (array (field_mut (Types.RefT absref))))
    in
    ([ ref_ argv ], Some argv)

and lower_block_type ctxt rep t : Types.block_type =
  match (t, rep) with
  | _, DropRep | Ast.TyTuple [], BlockRep _ -> Types.ValBlockType None
  | t, _ -> ValBlockType (Some (lower_value_type ctxt rep t))

(* Closure environments *)

let lower_clos_env ctxt vars : Types.field_type list =
  List.map
    (fun (_, t) -> field (lower_value_type ctxt (clos_rep ()) t))
    (Ast.VariableMap.bindings vars)
