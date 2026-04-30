module Wasm = Wasm.Ast
open Wasm.Types
module Source = Wasm.Source
module Ast = Language.Ast
open Utils.Utf8

(* Helpers *)

let i32 = int_of_string
let ( +% ) = Int32.add
let ( /% ) = Int32.div

(* Compilation context entities *)

type 'a entities = { mutable list : 'a option ref list; mutable cnt : int }

let make_entities () = { list = []; cnt = 0 }
let get_entities ents = List.rev (List.map (fun r -> Option.get !r) ents.list)

let alloc_entity ents : int * 'a option ref =
  let idx = ents.cnt in
  let r = ref None in
  ents.cnt <- idx + 1;
  ents.list <- r :: ents.list;
  (idx, r)

let define_entity r ent = r := Some ent

let emit_entity ents ent : int =
  let idx, r = alloc_entity ents in
  define_entity r ent;
  idx

let implicit_entity ents : int =
  assert (ents.list = []);
  let idx = ents.cnt in
  ents.cnt <- idx + 1;
  idx

(* Compilation context *)

module DefTypes = Map.Make (struct
  type t = sub_type

  let compare = compare
end)

module Refs = Set.Make (Int)
module Intrinsics = Map.Make (String)

type internal = {
  types : (sub_type, Ast.ty) Source.phrase entities;
  globals : Wasm.global entities;
  funcs : Wasm.func entities;
  memories : Wasm.memory entities;
  datas : Wasm.data_segment entities;
  imports : Wasm.import entities;
  exports : Wasm.export entities;
  locals : Wasm.local entities;
  instrs : Wasm.instr entities;
  refs : Refs.t ref;
  data_offset : int ref;
  start : int option ref;
  deftypes : int DefTypes.t ref;
  intrinsics : int Intrinsics.t ref;
}

type 'a ctxt = { ext : 'a; int : internal }

let make_internal () =
  {
    types = make_entities ();
    globals = make_entities ();
    funcs = make_entities ();
    memories = make_entities ();
    datas = make_entities ();
    imports = make_entities ();
    exports = make_entities ();
    locals = make_entities ();
    instrs = make_entities ();
    refs = ref Refs.empty;
    data_offset = ref 0;
    start = ref None;
    deftypes = ref DefTypes.empty;
    intrinsics = ref Intrinsics.empty;
  }

let make_ctxt ext = { ext; int = make_internal () }

(* Lookup *)

let lookup_sub_type ctxt idx : sub_type =
  (Option.get !(List.nth (List.rev ctxt.int.types.list) idx)).it

let lookup_str_type ctxt idx : str_type =
  let (SubT (_, _, st)) = lookup_sub_type ctxt idx in
  st

let lookup_func_type ctxt idx : func_type =
  match lookup_str_type ctxt idx with DefFuncT ft -> ft | _ -> assert false

let lookup_param_type ctxt idx i : val_type =
  let (FuncT (ts, _)) = lookup_func_type ctxt idx in
  List.nth ts i

let lookup_field_type ctxt idx i : val_type =
  match lookup_str_type ctxt idx with
  | DefStructT (StructT fts) -> (
      let (FieldT (_, t)) = List.nth fts i in
      match t with ValStorageT t -> t | _ -> assert false)
  | _ -> assert false

let lookup_ref_field_type ctxt idx i : int =
  match lookup_field_type ctxt idx i with
  | RefT (_, DefHT (DefT (_, idx'))) -> idx'
  | _ -> assert false

let lookup_intrinsic ctxt name f : int =
  match Intrinsics.find_opt name !(ctxt.int.intrinsics) with
  | Some idx -> idx
  | None ->
      let fwd = ref (-1) in
      let idx =
        f (fun idx ->
            ctxt.int.intrinsics :=
              Intrinsics.add name idx !(ctxt.int.intrinsics);
            fwd := idx)
      in
      assert (!fwd = -1 || !fwd = idx);
      if !fwd = -1 then
        ctxt.int.intrinsics := Intrinsics.add name idx !(ctxt.int.intrinsics);
      idx

(* Emitter *)

let emit_type ctxt dt : int =
  match DefTypes.find_opt dt !(ctxt.int.deftypes) with
  | Some idx -> idx
  | None ->
      let idx = emit_entity ctxt.int.types Source.(make_phrase dt) in
      ctxt.int.deftypes := DefTypes.add dt idx !(ctxt.int.deftypes);
      idx

let emit_type_deferred ctxt : int * (sub_type -> unit) =
  let idx, r = alloc_entity ctxt.int.types in
  ( idx,
    fun dt ->
      ctxt.int.deftypes := DefTypes.add dt idx !(ctxt.int.deftypes);
      define_entity r Source.(make_phrase dt) )

let emit_import ctxt mname name desc =
  let module_name = decode mname in
  let item_name = decode name in
  let idesc = desc in
  ignore (emit_entity ctxt.int.imports { module_name; item_name; idesc })

let emit_func_import ctxt mname name ft =
  let typeidx = emit_type ctxt (SubT (NoFinal, [], DefFuncT ft)) in
  emit_import ctxt mname name (FuncImport typeidx);
  implicit_entity ctxt.int.funcs

let emit_export descf ctxt name idx =
  let name = decode name in
  let edesc = descf idx in
  ignore (emit_entity ctxt.int.exports { name; edesc })

let emit_memory_import ctxt mname name min max =
  emit_import ctxt mname name (MemoryImport (MemoryT { min; max }));
  implicit_entity ctxt.int.memories

let emit_func_export ctxt = emit_export (fun x -> FuncExport x) ctxt
let emit_global_export ctxt = emit_export (fun x -> GlobalExport x) ctxt
let emit_memory_export ctxt = emit_export (fun x -> MemoryExport x) ctxt
let emit_param ctxt : int = implicit_entity ctxt.int.locals
let emit_local ctxt t' : int = emit_entity ctxt.int.locals t'

let emit_global ctxt mut t' ginit_opt : int =
  let gtype = GlobalT (mut, t') in
  let ginit =
    match ginit_opt with
    | Some ginit -> ginit
    | None -> (
        match t' with
        | NumT I32T -> [ Wasm.IntConst (I32T, 0) ]
        | NumT I64T -> [ IntConst (I64T, 0) ]
        | NumT F32T -> [ FloatConst (F32T, 0.0) ]
        | NumT F64T -> [ FloatConst (F64T, 0.0) ]
        | VecT V128T -> failwith "vector const not implemented"
        | RefT (Null, ht) -> [ RefNull ht ]
        | RefT (NoNull, _) -> assert false
        | BotT -> assert false)
  in
  emit_entity ctxt.int.globals { gtype; ginit }

let emit_memory ctxt min max : int =
  let mtype = MemoryT { min; max } in
  let idx = emit_entity ctxt.int.memories { mtype } in
  assert (idx = 0);
  idx

let emit_passive_data ctxt s : int =
  let dmode = Wasm.Passive in
  let seg = Wasm.{ dinit = s; dmode } in
  emit_entity ctxt.int.datas seg

let emit_active_data ctxt s : int =
  assert (get_entities ctxt.int.memories = []);
  let addr = !(ctxt.int.data_offset) in
  let offset = [ Wasm.IntConst (I32T, addr) ] in
  let dmode = Wasm.Active { index = 0; offset } in
  let seg = Wasm.{ dinit = s; dmode } in
  ignore (emit_entity ctxt.int.datas seg);
  ctxt.int.data_offset := addr + String.length s;
  addr

let emit_instr ctxt instr = ignore (emit_entity ctxt.int.instrs instr)

let emit_block ctxt head bt f =
  let ctxt' = { ctxt with int = { ctxt.int with instrs = make_entities () } } in
  f ctxt';
  emit_instr ctxt (head bt (get_entities ctxt'.int.instrs))

let emit_func_deferred ctxt : int * _ =
  let idx, func = alloc_entity ctxt.int.funcs in
  ( idx,
    fun ctxt ts1' ts2' f ->
      let ft = FuncT (ts1', ts2') in
      let typeidx = emit_type ctxt (SubT (NoFinal, [], DefFuncT ft)) in
      let ctxt' =
        {
          ctxt with
          int =
            {
              ctxt.int with
              locals = make_entities ();
              instrs = make_entities ();
            };
        }
      in
      f ctxt' idx;
      define_entity func
        {
          ftype = typeidx;
          locals = get_entities ctxt'.int.locals;
          body = get_entities ctxt'.int.instrs;
        } )

let emit_func ctxt ts1' ts2' f : int =
  let idx, def_func = emit_func_deferred ctxt in
  def_func ctxt ts1' ts2' f;
  idx

let emit_func_ref ctxt idx = ctxt.int.refs := Refs.add idx !(ctxt.int.refs)

let emit_start ctxt idx =
  assert (!(ctxt.int.start) = None);
  ctxt.int.start := Some idx

let compact s = Scc.IntSet.(min_elt s + cardinal s = max_elt s + 1)

let recify sts =
  let sta = Array.of_list sts in
  let sccs = Scc.sccs_of_subtypes (Array.map Source.it sta) in
  assert (List.for_all compact sccs);
  List.map
    (fun scc ->
      match Scc.IntSet.elements scc with
      | [ x ] -> RecT [ sta.(x).it ]
      | xs -> RecT (List.map (fun x -> sta.(x).it) xs))
    (List.sort Scc.IntSet.compare sccs)

(* Generation *)

let gen_module ctxt : Wasm.module_ =
  {
    Wasm.empty_module with
    Wasm.start =
      (match !(ctxt.int.start) with
      | None -> None
      | Some x -> Some { sfunc = x });
    Wasm.types = recify (get_entities ctxt.int.types);
    Wasm.globals = get_entities ctxt.int.globals;
    Wasm.funcs = get_entities ctxt.int.funcs;
    Wasm.imports = get_entities ctxt.int.imports;
    Wasm.exports = get_entities ctxt.int.exports;
    Wasm.datas = get_entities ctxt.int.datas;
    Wasm.elems =
      (if !(ctxt.int.refs) = Refs.empty then []
       else
         Wasm.
           [
             {
               etype = (NoNull, FuncHT);
               emode = Declarative;
               einit =
                 Refs.fold
                   (fun idx consts -> [ RefFunc idx ] :: consts)
                   !(ctxt.int.refs) [];
             };
           ]);
    Wasm.memories =
      (let memories = get_entities ctxt.int.memories in
       if get_entities ctxt.int.datas = [] || ctxt.int.memories.cnt > 0 then
         memories
       else
         let sz = (!(ctxt.int.data_offset) + 0xffff) / 0x10000 in
         [ { Wasm.mtype = MemoryT { min = sz; max = Some sz } } ]);
  }
