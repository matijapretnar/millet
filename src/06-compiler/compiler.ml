module Const = Language.Const
module Typed = Typechecker.TypedAst
module Ast = Language.Ast
module Ty = Typechecker
module Lower = Lower
module Env = Env
module Types = Wasm.Types
module Arrange = Wasm.Arrange
module Sexpr = Wasm.Sexpr
module Wasm = Wasm.Ast
module Primitives = Primitives
open Emit
open Source

type pat_class = IrrelevantPat | TotalPat | PartialPat

type comp_state = {
  cmds : Typed.command list;
  prims : (Ast.variable * Language.Primitives.primitive) list;
}

let max_func_arity = Lower.max_func_arity
let ref_cast idx = Wasm.RefCast (Types.NoNull, Types.VarHT (Types.StatX idx))

let rec find_var f ctxt (x : Ast.variable)
    (envs : (Lower.scope * Lower.env ref) list) : Lower.(loc * func_loc option)
    =
  match envs with
  | [] ->
      Ast.Variable.print x Format.str_formatter;
      Printf.printf "[find_val_var `%s`]\n%!" (Format.flush_str_formatter ());
      failwith "false"
  | (_, env) :: envs' -> (
      match f x !env with
      | Some { it = func_loc_opt; _ } -> func_loc_opt
      | None -> find_var f ctxt x envs')

let rec find_prim ctxt (x : Ast.variable)
    (envs : (Lower.scope * Lower.env ref) list) =
  match envs with
  | [] ->
      Ast.Variable.print x Format.str_formatter;
      Printf.printf "[find_func `%s`]\n%!" (Format.flush_str_formatter ());
      failwith "false"
  | (_, env) :: envs' -> (
      match Env.find_opt_prim x !env with
      | Some prim -> prim
      | None -> find_prim ctxt x envs')

let find_val_var = find_var Env.find_opt_val

let rec find_var_opt f ctxt (x : Ast.variable)
    (envs : (Lower.scope * Lower.env ref) list) :
    Lower.(loc * func_loc option) option =
  match envs with
  | [] -> None
  | (_, env) :: envs' -> (
      match f x !env with
      | Some { it = func_loc_opt; _ } -> Some func_loc_opt
      | None -> find_var_opt f ctxt x envs')

let find_val_var_opt = find_var_opt Env.find_opt_val

let find_val_var_or_prim ctxt (var : Ast.variable)
    (envs : (Lower.scope * Lower.env ref) list) =
  match find_val_var_opt ctxt var envs with
  | Some x -> x
  | None -> (Lower.GlobalLoc (-1), None)

let filter_loc (ctxt : Lower.ctxt) vals =
  Ast.VariableMap.filter
    (fun x _ ->
      match find_val_var_or_prim ctxt x ctxt.ext.envs with
      | Lower.GlobalLoc _, _ -> false
      | (Lower.LocalLoc _ | Lower.ClosureLoc _), _ -> true)
    vals

let filter_vars (ctxt : Lower.ctxt) vars = filter_loc ctxt vars

(* coerce *)

let rec compile_coerce ctxt src dst t =
  if src <> dst then
    let emit ctxt = List.iter (Emit.emit_instr ctxt) in
    let non_null n1 n2 =
      if n1 = Lower.Null && n2 = Lower.Nonull then emit ctxt [ RefAsNonNull ]
    in
    match (src, dst) with
    | Lower.BlockRep _, DropRep when Ty.eq t (Ast.TyTuple []) -> ()
    | _, DropRep -> emit ctxt [ Drop ]
    | _, BlockRep _ when Ty.eq t (Ast.TyTuple []) -> emit ctxt [ Drop ]
    | BlockRep _, _ when Ty.eq t (Ast.TyTuple []) ->
        emit ctxt [ IntConst (I32T, 0); RefI31 ]
    | (BlockRep n1 | BoxedRep n1 | BoxedAbsRep n1), BoxedAbsRep n2
    | (BlockRep n1 | BoxedRep n1), (BlockRep n2 | BoxedRep n2) ->
        non_null n1 n2
    | BoxedAbsRep n1, (BlockRep n2 | BoxedRep n2) -> (
        match t with
        | Ast.TyConst Const.BooleanTy | Ast.TyConst Const.IntegerTy ->
            emit ctxt [ RefCast (NoNull, I31HT) ]
        | Ast.TyConst Const.FloatTy ->
            let boxedfloat =
              Lower.lower_var_type ctxt (Ast.TyConst Const.FloatTy)
            in
            emit ctxt [ ref_cast boxedfloat ]
        | Ast.TyTuple [] | Ast.TyApply _ | Ast.TyParam _ -> non_null n1 n2
        | TyArrow _ ->
            let x = snd (Lower.lower_func_type ctxt 1) in
            non_null n1 n2;
            emit ctxt [ ref_cast x ]
        | t ->
            (* No types handled here can use super RTTs *)
            let x = Lower.lower_var_type ctxt t in
            non_null n1 n2;
            emit ctxt [ ref_cast x ])
    | ( (BlockRep _ | BoxedRep _ | BoxedAbsRep _),
        (UnboxedRep n2 | UnboxedLaxRep n2) ) -> (
        compile_coerce ctxt src (BoxedRep n2) t;
        match t with
        | Ast.TyConst Const.BooleanTy -> emit ctxt [ I31Get ZX ]
        | Ast.TyConst Const.IntegerTy -> emit ctxt [ I31Get SX ]
        | Ast.TyConst Const.FloatTy ->
            let boxedfloat =
              Lower.lower_var_type ctxt (Ast.TyConst Const.FloatTy)
            in
            emit ctxt [ Wasm.StructGet (boxedfloat, 0, None) ]
        | _ -> ())
    | ( (UnboxedRep n1 | UnboxedLaxRep n1),
        (BlockRep n2 | BoxedRep n2 | BoxedAbsRep n2) ) -> (
        match t with
        | Ast.TyConst Const.BooleanTy | Ast.TyConst Const.IntegerTy ->
            emit ctxt [ RefI31 ]
        | Ast.TyConst Const.FloatTy ->
            let boxedfloat =
              Lower.lower_var_type ctxt (Ast.TyConst Const.FloatTy)
            in
            emit ctxt [ StructNew (boxedfloat, Explicit) ]
        | _ -> non_null n1 n2)
    | UnboxedLaxRep n1, UnboxedRep n2 -> (
        match t with
        | Ast.TyConst Const.IntegerTy ->
            emit ctxt
              [ IntConst (I32T, 1); Shl I32T; IntConst (I32T, 1); Shr I32T ]
        | Ast.TyConst Const.BooleanTy | Ast.TyConst Const.FloatTy -> ()
        | _ -> non_null n1 n2)
    | (UnboxedRep n1 | UnboxedLaxRep n1), (UnboxedRep n2 | UnboxedLaxRep n2)
      -> (
        match t with
        | Ast.TyConst Const.BooleanTy
        | Ast.TyConst Const.IntegerTy
        | Ast.TyConst Const.FloatTy ->
            ()
        | _ -> non_null n1 n2)
    | DropRep, _ -> assert false

(* helpers *)

let curry_fun_idx = Lower.clos_env_idx
let curry_arg_idx = Lower.clos_env_idx + 1 (* first argument or argv *)

let compile_push_args ctxt n shift compile_eI =
  let emit ctxt = List.iter (emit_instr ctxt) in
  match Lower.lower_param_types ctxt n with
  | _, None ->
      for i = 0 to n - 1 do
        compile_eI i
      done
  | _, Some argv ->
      let tmp = emit_local ctxt { ltype = RefT (NoNull, VarHT (StatX argv)) } in
      emit ctxt
        [
          IntConst (I32T, 0);
          RefI31;
          IntConst (I32T, n);
          ArrayNew (argv, Explicit);
          LocalTee (tmp + shift);
          RefAsNonNull;
        ];
      for i = 0 to n - 1 do
        emit ctxt [ LocalGet (tmp + shift); IntConst (I32T, i) ];
        compile_eI i;
        emit ctxt [ ArraySet argv ]
      done

(* Constants *)
let lower_text_type ctxt : int =
  emit_type ctxt
    (SubT (NoFinal, [], DefArrayT (ArrayT (FieldT (Var, PackStorageT Pack8)))))

let compile_text_new ctxt : int =
  let i32_load8_u align offset =
    Wasm.Load { ty = I32T; align; offset; pack = Some (Pack8, ZX) }
  in
  let block bt es = Wasm.Block (bt, es) in
  let loop bt es = Wasm.Loop (bt, es) in
  Emit.lookup_intrinsic ctxt "text_new" (fun _ ->
      let text = lower_text_type ctxt in
      let textref = Types.RefT (NoNull, VarHT (StatX text)) in
      let textnullref =
        Wasm.{ ltype = Types.RefT (Null, VarHT (StatX text)) }
      in
      emit_func ctxt [ NumT I32T; NumT I32T ] [ textref ] (fun ctxt _ ->
          let src = emit_param ctxt in
          let len = emit_param ctxt in
          let dst = emit_local ctxt textnullref in
          List.iter (emit_instr ctxt)
            [
              LocalGet len;
              ArrayNew (text, Implicit);
              LocalSet dst;
              block (Types.ValBlockType None)
                (List.map
                   (fun e -> e)
                   Wasm.
                     [
                       loop (Types.ValBlockType None)
                         (List.map
                            (fun e -> e)
                            [
                              LocalGet len;
                              Eqz I32T;
                              BrIf 1;
                              LocalGet dst;
                              LocalGet len;
                              IntConst (I32T, 1);
                              Sub I32T;
                              LocalTee len;
                              LocalGet len;
                              LocalGet src;
                              Add I32T;
                              i32_load8_u 0 0;
                              ArraySet text;
                              Br 0;
                            ]);
                     ]);
              LocalGet dst;
              RefAsNonNull;
            ]))

let compile_lit ctxt l =
  let emit ctxt = List.iter (emit_instr ctxt) in
  match l with
  | Const.Boolean b -> emit ctxt [ IntConst (I32T, if b then 1 else 0) ]
  | Integer i -> emit ctxt Wasm.[ IntConst (I32T, i) ]
  | Float z -> emit ctxt Wasm.[ FloatConst (F64T, z) ]
  | String s ->
      let addr = emit_active_data ctxt s in
      emit ctxt [ IntConst (I32T, addr) ];
      emit ctxt
        [ IntConst (I32T, String.length s); Call (compile_text_new ctxt) ]

let compile_load_arg ctxt i arg argv_opt =
  let emit ctxt = List.iter (emit_instr ctxt) in
  match argv_opt with
  | None ->
      assert (i < max_func_arity);
      emit ctxt [ LocalGet (arg - 1 + i) ]
  | Some argv ->
      emit ctxt
        [
          LocalGet (arg - 1);
          IntConst (I32T, i);
          ArrayGet (argv, None);
          RefAsNonNull;
        ]

let compile_load_args ctxt i j shift arg0 src_argv_opt =
  assert (j <= max_func_arity || src_argv_opt <> None);
  if j - i > max_func_arity && i = 0 then
    (* Reuse argv *)
    emit_instr ctxt (LocalGet (arg0 + shift))
  else
    compile_push_args ctxt (j - i) shift (fun k ->
        compile_load_arg ctxt (i + k) (arg0 + shift) src_argv_opt)

let rec compile_func_apply arity ctxt =
  assert (arity > 0);
  Emit.lookup_intrinsic ctxt
    ("func_apply" ^ string_of_int arity)
    (fun def_fwd ->
      let emit ctxt = List.iter (emit_instr ctxt) in
      let anyclos = Lower.lower_anyclos_type ctxt in
      let argts, argv_opt = Lower.lower_param_types ctxt arity in
      let func_idx =
        emit_func ctxt
          (RefT (NoNull, VarHT (StatX anyclos)) :: argts)
          [ RefT Lower.absref ]
          (fun ctxt fn ->
            def_fwd fn;
            let clos = emit_param ctxt in
            let args = List.map (fun _ -> emit_param ctxt) argts in
            let arg0 = List.hd args in
            let block bt es = Wasm.Block (bt, es) in
            emit_block ctxt block (ValBlockType None) (fun ctxt ->
                emit_block ctxt block (ValBlockType None) (fun ctxt ->
                    let rec over_apply ctxt = function
                      | 0 ->
                          (* Dispatch on closure arity *)
                          let labs =
                            List.init (arity + 1) (fun i -> max 0 (i - 1))
                          in
                          emit ctxt
                            [
                              LocalGet clos;
                              StructGet (anyclos, Lower.clos_arity_idx, None);
                              BrTable (labs, arity);
                            ]
                      | n ->
                          emit_block ctxt block (ValBlockType None) (fun ctxt ->
                              over_apply ctxt (n - 1));

                          (* Dispatching here when closure arity n < apply arity *)
                          let codeN, closN = Lower.lower_func_type ctxt n in
                          let ref_closN_0 =
                            emit_local ctxt
                              { ltype = RefT (NoNull, VarHT (StatX closN)) }
                          in
                          (* Downcast closure type *)
                          emit ctxt
                            [
                              LocalGet clos;
                              ref_cast closN;
                              LocalSet ref_closN_0;
                            ];
                          emit_block ctxt block (ValBlockType None) (fun ctxt ->
                              (* Call target function with arguments it can handle *)
                              emit ctxt [ LocalGet ref_closN_0 ];
                              if n == 1 then emit ctxt [ LocalGet 1 ]
                              else compile_load_args ctxt 0 n 1 arg0 argv_opt;

                              emit ctxt
                                [
                                  LocalGet ref_closN_0;
                                  StructGet (closN, Lower.clos_code_idx, None);
                                  CallRef codeN;
                                  (* Downcast resulting closure *)
                                  ref_cast anyclos;
                                ];
                              (* Apply result to remaining arguments *)
                              compile_load_args ctxt n arity 1 arg0 argv_opt;
                              emit ctxt
                                [
                                  Call (compile_func_apply (arity - n) ctxt);
                                  Return;
                                  (* TODO: should be tail call *)
                                ])
                    in
                    over_apply ctxt (arity - 1));
                (* Dispatching here when closure arity n = apply arity *)
                let codeN, closN = Lower.lower_func_type ctxt arity in
                let ref_closN_1 =
                  emit_local ctxt { ltype = RefT (NoNull, VarHT (StatX closN)) }
                in
                (* Downcast closure type *)
                emit ctxt
                  [ LocalGet clos; ref_cast closN; LocalSet ref_closN_1 ];
                (* Bind result to local *)
                emit_block ctxt block (ValBlockType None) (fun ctxt ->
                    (* Call target function *)
                    emit ctxt [ LocalGet ref_closN_1 ];
                    if arity > max_func_arity then emit ctxt [ LocalGet 1 ]
                    else compile_load_args ctxt 0 arity 1 arg0 argv_opt;
                    emit ctxt
                      [
                        LocalGet ref_closN_1;
                        StructGet (closN, Lower.clos_code_idx, None);
                        CallRef codeN;
                        Return;
                        (* TODO: should be tail call *)
                      ]));
            (* Dispatching here when closure arity > apply arity *)
            (* Create curried closure *)
            let flds =
              List.map Lower.field
                (RefT (NoNull, VarHT (StatX anyclos)) :: argts)
            in
            let _, _, curriedN = Lower.lower_clos_type ctxt 1 flds in
            emit ctxt
              [
                IntConst (I32T, 1);
                RefFunc (compile_func_curry arity ctxt);
                LocalGet clos;
                (*LocalGet 1;*)
              ];
            if arity > max_func_arity then emit ctxt [ LocalGet 1 ]
            else compile_load_args ctxt 0 arity 1 arg0 argv_opt;
            emit ctxt [ StructNew (curriedN, Explicit) ])
      in
      (*print_endline ("func_apply" ^ string_of_int arity);
    print_endline (string_of_int func_idx);*)
      func_idx)

and compile_func_curry arity ctxt =
  let block bt es = Wasm.Block (bt, es) in
  let loop bt es = Wasm.Loop (bt, es) in
  let arity_string =
    if arity <= max_func_arity then string_of_int arity else "vec"
  in
  Emit.lookup_intrinsic ctxt ("func_curry" ^ arity_string) (fun def_fwd ->
      let emit ctxt = List.iter (emit_instr ctxt) in
      let anyclos = Lower.lower_anyclos_type ctxt in
      let argts, argv_opt = Lower.lower_param_types ctxt arity in
      let flds =
        List.map Lower.field (RefT (NoNull, VarHT (StatX anyclos)) :: argts)
      in
      let _, clos1, curriedN = Lower.lower_clos_type ctxt 1 flds in
      (* curryN = fun xN => apply_N+1 x0 ... xN-1 xN *)
      emit_func ctxt
        [ RefT (NoNull, VarHT (StatX clos1)); RefT Lower.absref ]
        [ RefT Lower.absref ]
        (fun ctxt fn ->
          def_fwd fn;
          let clos = emit_param ctxt in
          let arg0 = emit_param ctxt in
          let ref_curriedN =
            emit_local ctxt { ltype = RefT (NoNull, VarHT (StatX curriedN)) }
          in
          emit_func_ref ctxt fn;
          (* Downcast generic to specific closure type *)
          emit ctxt [ LocalGet clos; ref_cast curriedN; LocalSet ref_curriedN ];
          (* Bind result to local *)
          emit_block ctxt block (ValBlockType (Some (RefT Lower.absref)))
            (fun ctxt ->
              (* Load arguments *)
              if arity <= max_func_arity then begin
                (* Load target function *)
                emit ctxt
                  [
                    LocalGet ref_curriedN;
                    StructGet (curriedN, curry_fun_idx, None);
                  ];
                (* Target expects direct args, load them individually *)
                compile_push_args ctxt (arity + 1) 0 (fun i ->
                    if i < arity then
                      (* First arity args are loaded from closure environment *)
                      emit ctxt
                        [
                          LocalGet ref_curriedN;
                          StructGet (curriedN, curry_arg_idx + i, None);
                        ]
                    else
                      (* Last arg is the applied one *)
                      emit ctxt [ LocalGet arg0 ]);
                (* Call target *)
                emit ctxt
                  [
                    Call (compile_func_apply (arity + 1) ctxt);
                    Return;
                    (* TODO: should be tail call *)
                  ]
              end
              else begin
                (* Target expects arg vector, copy to extended vector *)
                (* This code is agnostic to static arity, hence works for any *)
                let argv = Option.get argv_opt in
                let src =
                  emit_local ctxt { ltype = RefT (NoNull, VarHT (StatX argv)) }
                in
                let dst =
                  emit_local ctxt { ltype = RefT (NoNull, VarHT (StatX argv)) }
                in
                let len = emit_local ctxt { ltype = NumT I32T } in
                let i = emit_local ctxt { ltype = NumT I32T } in
                emit ctxt
                  [
                    (* Array init value *)
                    IntConst (I32T, 0);
                    RefI31;
                    (* Load source *)
                    LocalGet ref_curriedN;
                    (* curriedN *)
                    StructGet (curriedN, curry_arg_idx, None);
                    LocalTee src;
                    (* Load length *)
                    ArrayLen;
                    LocalTee i;
                    (* Allocate destination *)
                    IntConst (I32T, 1);
                    Add I32T;
                    LocalTee len;
                    ArrayNew (argv, Explicit);
                    LocalTee dst;
                    (* Initialise applied argument *)
                    LocalGet i;
                    LocalGet arg0;
                    ArraySet argv;
                  ];
                (* Copy closure arguments, from upper to lower *)
                emit_block ctxt block (ValBlockType None) (fun ctxt ->
                    emit_block ctxt loop (ValBlockType None) (fun ctxt ->
                        emit ctxt
                          [
                            (* Check termination condition *)
                            LocalGet i;
                            Eqz I32T;
                            BrIf 1;
                            (* Load destination *)
                            LocalGet dst;
                            (* Compute next index *)
                            LocalGet i;
                            IntConst (I32T, 1);
                            Sub I32T;
                            LocalTee i;
                            (* Read arg value from source *)
                            LocalGet src;
                            LocalGet i;
                            ArrayGet (argv, None);
                            (* Store arg value to destination *)
                            ArraySet argv;
                            (* Iterate *)
                            Br 0;
                          ]));
                emit_block ctxt block (ValBlockType None) (fun ctxt ->
                    let codeN, closNP =
                      Lower.lower_func_type ctxt (arity + 1)
                    in
                    let ref_closNP =
                      emit_local ctxt
                        { ltype = RefT (NoNull, VarHT (StatX closNP)) }
                    in
                    emit ctxt
                      [
                        (* Load source arity *)
                        LocalGet len;
                        (* Load destination arity *)
                        LocalGet ref_curriedN;
                        StructGet (curriedN, curry_fun_idx, None);
                        StructGet (anyclos, Lower.clos_arity_idx, None);
                        (* Compare *)
                        Ne I32T;
                        BrIf 0;
                        (* All arguments collected, perform call *)
                        LocalGet ref_curriedN;
                        StructGet (curriedN, curry_fun_idx, None);
                        ref_cast closNP;
                        LocalTee ref_closNP;
                        LocalGet dst;
                        RefAsNonNull;
                        LocalGet ref_closNP;
                        StructGet (closNP, Lower.clos_code_idx, None);
                        CallRef codeN;
                        Return;
                        (* TODO: should be tail call *)
                      ]);
                (* Still missing arguments, create new closure *)
                emit ctxt
                  [
                    IntConst (I32T, 1);
                    RefFunc (compile_func_curry arity ctxt);
                    LocalGet ref_curriedN;
                    StructGet (curriedN, curry_fun_idx, None);
                    LocalGet dst;
                    RefAsNonNull;
                    (*rtt_canon (curriedN );*)
                    StructNew (curriedN, Explicit);
                  ]
              end)))

(* strings *)

let lower_text_type ctxt : int =
  emit_type ctxt
    (SubT (NoFinal, [], DefArrayT (ArrayT (FieldT (Var, PackStorageT Pack8)))))

let compile_text_eq ctxt : int =
  let block bt es = Wasm.Block (bt, es) in
  let loop bt es = Wasm.Loop (bt, es) in
  let if_ bt es1 es2 = Wasm.If (bt, es1, es2) in
  Emit.lookup_intrinsic ctxt "text_eq" (fun _ ->
      let text = lower_text_type ctxt in
      let textref = Types.RefT (NoNull, VarHT (StatX text)) in
      emit_func ctxt [ textref; textref ] [ NumT I32T ] (fun ctxt _ ->
          let arg1 = emit_param ctxt in
          let arg2 = emit_param ctxt in
          let len = emit_local ctxt { ltype = NumT I32T } in
          List.iter (emit_instr ctxt)
            [
              block (ValBlockType None)
                (List.map
                   (fun e -> e)
                   Wasm.
                     [
                       LocalGet arg1;
                       LocalGet arg2;
                       RefEq;
                       if_ (ValBlockType None)
                         (List.map (fun e -> e) [ IntConst (I32T, 1); Return ])
                         [];
                       LocalGet arg1;
                       ArrayLen;
                       LocalGet arg2;
                       ArrayLen;
                       LocalTee len;
                       Ne I32T;
                       BrIf 0;
                       block (ValBlockType None)
                         (List.map
                            (fun e -> e)
                            [
                              loop (ValBlockType None)
                                (List.map
                                   (fun e -> e)
                                   [
                                     LocalGet len;
                                     Eqz I32T;
                                     BrIf 1;
                                     LocalGet arg1;
                                     LocalGet len;
                                     IntConst (I32T, 1);
                                     Sub I32T;
                                     LocalTee len;
                                     ArrayGet (text, Some ZX);
                                     LocalGet arg2;
                                     LocalGet len;
                                     ArrayGet (text, Some ZX);
                                     Ne I32T;
                                     BrIf 2;
                                     Br 0;
                                   ]);
                            ]);
                       IntConst (I32T, 1);
                       Return;
                     ]);
              IntConst (I32T, 0);
            ]))

(* variables *)

let compile_var find_var (ctxt : Lower.ctxt) x =
  let loc, funcloc_opt = find_var ctxt x ctxt.ext.envs in
  (match loc with
  | Lower.LocalLoc idx -> emit_instr ctxt (Wasm.LocalGet idx)
  | Lower.GlobalLoc idx -> emit_instr ctxt (Wasm.GlobalGet idx)
  | Lower.ClosureLoc (null, idx, localidx, typeidx) ->
      emit_instr ctxt (LocalGet localidx);
      emit_instr ctxt (StructGet (typeidx, idx, None));
      if null = Null then emit_instr ctxt RefAsNonNull);
  (loc, funcloc_opt)

let compile_val_var ctxt x t dst =
  let loc, funcloc_opt = compile_var find_val_var ctxt x in
  let rep = Lower.loc_rep loc in
  match funcloc_opt with
  | None -> compile_coerce ctxt rep dst t
  | Some (_ : Lower.func_loc) ->
      if Lower.null_rep rep = Null && Lower.null_rep dst <> Null then
        emit_instr ctxt Wasm.RefAsNonNull

let compile_val_var_bind_pre ctxt x t funcloc_opt =
  let scope, env = Lower.current_scope ctxt in
  let rep = Lower.scope_rep scope in
  let vt =
    match funcloc_opt with
    | None -> (
        match t with
        | Ast.TyArrow _ ->
            Lower.lower_value_type ctxt rep
              (Ast.TyArrow
                 ( TyParam (Ast.TyParam.fresh "func"),
                   TyParam (Ast.TyParam.fresh "func") ))
        | _ -> Lower.lower_value_type ctxt rep t)
    | Some ({ typeidx; _ } : Lower.func_loc) ->
        if Lower.null_rep rep = Null then RefT (Null, VarHT (StatX typeidx))
        else RefT (NoNull, VarHT (StatX typeidx))
  in
  let loc =
    match scope with
    | PreScope -> assert false
    | LocalScope -> Lower.LocalLoc (emit_local ctxt { ltype = vt })
    | GlobalScope -> Lower.GlobalLoc (emit_global ctxt Var vt None)
  in
  env := Env.extend_val !env x (make_phrase (loc, funcloc_opt))

let compile_val_var_bind_post ctxt x t src =
  let _, env = Lower.current_scope ctxt in
  let loc, _ = (Env.find_val x !env).it in
  compile_coerce ctxt src (Lower.loc_rep loc) t;
  match loc with
  | ClosureLoc _ -> assert false
  | LocalLoc idx -> emit_instr ctxt (LocalSet idx)
  | GlobalLoc idx -> emit_instr ctxt (GlobalSet idx)

let compile_val_var_bind ctxt x t src funcloc_opt =
  compile_val_var_bind_pre ctxt x t funcloc_opt;
  compile_val_var_bind_post ctxt x t src

(* closures *)

let local_ref_ty idx =
  let x = Types.RefT (NoNull, VarHT (StatX idx)) in
  Wasm.{ ltype = x }

let compile_load_env ctxt clos (closNenv : int) vars envflds =
  if vars <> Ast.VariableMap.empty then begin
    let emit ctxt = List.iter (emit_instr ctxt) in
    let envlocal = emit_local ctxt (local_ref_ty closNenv) in
    let rttidx = closNenv in
    emit ctxt [ LocalGet clos; ref_cast rttidx; LocalSet envlocal ];
    let _, env = Lower.current_scope ctxt in
    let _ =
      Ast.VariableMap.fold
        (fun x _ i ->
          let idx = Lower.clos_env_idx + i in
          let _, func_loc_opt = find_val_var ctxt x ctxt.ext.envs in
          let null =
            if func_loc_opt = None then Lower.Nonull
            else
              match List.nth envflds i with
              | Types.FieldT (_, ValStorageT (RefT (Null, _))) -> Null
              | _ -> Nonull
          in
          env :=
            Env.extend_val !env x
              (make_phrase
                 (Lower.ClosureLoc (null, idx, envlocal, closNenv), func_loc_opt));
          i + 1)
        vars 0
    in
    ()
  end

let compile_alloc_clos ctxt fn arity vars closNenv =
  let emit ctxt = List.iter (emit_instr ctxt) in
  (* Emit a reference to the function *)
  emit_func_ref ctxt fn;
  (* Push the arity and the function reference onto the stack *)
  emit ctxt
    [
      IntConst (I32T, arity);
      (* Push the function's arity *)
      RefFunc fn;
      (* Push a reference to the function *)
    ];
  (* Iterate over captured variables and compile them *)
  Ast.VariableMap.iter
    (fun x t ->
      let rep = Lower.clos_rep () in
      (* Always use the closure representation *)
      compile_val_var ctxt x t
        rep (* Compile the variable with closure representation *))
    vars;
  (* Create the closure structure *)
  emit ctxt [ StructNew (closNenv, Explicit) ]

(* patterns *)

let rec classify_pat p =
  match p.it with
  | Typed.PVar _ -> TotalPat
  | Typed.PAnnotated (p1, _) -> classify_pat p1
  | Typed.PAs (p1, _) -> max (classify_pat p1) TotalPat
  | Typed.PTuple ps ->
      List.fold_left max IrrelevantPat (List.map classify_pat ps)
  | Typed.PVariant _ -> PartialPat
  | Typed.PConst _ -> PartialPat
  | Typed.PNonbinding -> IrrelevantPat

let rec compile_pattern ctxt (fail : int) (pat : Typed.pattern) funcloc_opt =
  let emit ctxt = List.iter (emit_instr ctxt) in
  match pat.it with
  | Typed.PNonbinding | Typed.PTuple [] ->
      compile_coerce ctxt (Lower.pat_rep ()) DropRep (Ast.TyTuple [])
  | Typed.PVar var ->
      compile_val_var_bind ctxt var (et pat) (Lower.pat_rep ()) funcloc_opt
  | Typed.PAnnotated (p, _) -> compile_pattern ctxt fail p funcloc_opt
  | Typed.PAs (p, var) ->
      let typeidx = Lower.lower_var_type ctxt (et pat) in
      let tmp =
        emit_local ctxt { ltype = RefT (Types.NoNull, VarHT (StatX typeidx)) }
      in
      compile_coerce ctxt (Lower.pat_rep ()) Lower.rigid_rep (et pat);
      emit ctxt [ LocalTee tmp ];
      compile_pattern ctxt fail p funcloc_opt;
      emit ctxt [ LocalGet tmp ];
      compile_val_var_bind ctxt var (et p) (Lower.pat_rep ()) funcloc_opt
  | Typed.PTuple ps ->
      let typeidx = Lower.lower_var_type ctxt (et pat) in
      let tmp =
        emit_local ctxt { ltype = RefT (Types.NoNull, VarHT (StatX typeidx)) }
      in
      compile_coerce ctxt (Lower.pat_rep ()) Lower.rigid_rep (et pat);
      emit ctxt [ LocalSet tmp ];
      List.iteri
        (fun i pI ->
          if classify_pat pI > IrrelevantPat then begin
            emit ctxt [ LocalGet tmp ];
            emit ctxt [ StructGet (typeidx, i, None) ];
            compile_coerce ctxt Lower.field_rep (Lower.pat_rep ()) (et pI);
            compile_pattern ctxt fail pI funcloc_opt
          end)
        ps
  | Typed.PVariant (lbl, p) -> (
      match Source.et pat with
      | Ast.TyApply (y, _) -> (
          let func ts1 ts2 = Types.DefFuncT (FuncT (ts1, ts2)) in
          let block bt es = Wasm.Block (bt, es) in
          let data = Lower.find_typ_var ctxt y ctxt.ext.envs in
          let con = List.assoc lbl data in
          let con_ty = (Types.NoNull, Types.VarHT (StatX con.typeidx)) in
          let tmp = emit_local ctxt { ltype = RefT con_ty } in
          let bt2 =
            emit_type ctxt
              (Lower.sub []
                 (func [ RefT Lower.absref ] [ Lower.ref_ con.typeidx ]))
          in
          emit_block ctxt block (VarBlockType bt2) (fun ctxt ->
              emit ctxt [ BrOnCast (0, Lower.absref, con_ty); Br (fail + 1) ]);
          emit ctxt
            [
              LocalTee tmp;
              StructGet (con.typeidx, 0, None);
              IntConst (I32T, con.tag);
              Ne I32T;
              BrIf fail;
            ];
          match p with
          | None -> ()
          | Some x ->
              emit ctxt [ LocalGet tmp; StructGet (con.typeidx, 1, None) ];
              compile_coerce ctxt Lower.field_rep (Lower.pat_rep ()) (et x);
              compile_pattern ctxt fail x funcloc_opt)
      | _ -> assert false)
  | Typed.PConst const ->
      compile_coerce ctxt (Lower.pat_rep ()) Lower.rigid_rep (et pat);
      compile_lit ctxt const;
      (match et pat with
      | Ast.TyConst IntegerTy | Ast.TyConst BooleanTy ->
          emit ctxt [ Wasm.Ne I32T ]
      | Ast.TyConst FloatTy -> emit ctxt [ Wasm.Ne F64T ]
      | Ast.TyConst StringTy ->
          emit ctxt [ Call (compile_text_eq ctxt); Eqz I32T ]
      | _ -> assert false);
      emit ctxt [ BrIf fail ]

(* expressions *)
and compile_expression (ctxt : 'a ctxt) (exp : Typed.expression) dst :
    Lower.func_loc option =
  let emit ctxt = List.iter (emit_instr ctxt) in
  let t = et exp in
  match it exp with
  | Typed.Var var ->
      compile_val_var ctxt var t dst;
      let _, func_loc_opt = find_val_var ctxt var ctxt.ext.envs in
      func_loc_opt
  | Const c ->
      compile_lit ctxt c;
      compile_coerce ctxt Lower.rigid_rep dst t;
      None
  | Annotated (exp1, _) -> compile_expression ctxt exp1 dst
  | Tuple [] ->
      compile_coerce ctxt Lower.unit_rep dst (Ast.TyTuple []);
      None
  | Tuple es ->
      let typ = Lower.lower_var_type ctxt t in
      List.iter (fun e -> ignore (compile_expression ctxt e Lower.field_rep)) es;
      emit ctxt [ StructNew (typ, Explicit) ];
      compile_coerce ctxt Lower.rigid_rep dst t;
      None
  | Variant (lbl, e) ->
      (match Source.et exp with
      | Ast.TyApply (y, _) ->
          let data = Lower.find_typ_var ctxt y ctxt.ext.envs in
          let con = List.assoc lbl data in
          emit ctxt [ IntConst (I32T, con.tag) ];
          (match e with
          | None -> ()
          | Some x -> ignore (compile_expression ctxt x Lower.rigid_rep));
          emit ctxt [ StructNew (con.typeidx, Explicit) ]
      | _ -> assert false);
      None
  | Lambda abs -> Some (compile_func ctxt abs)
  | RecLambda (var, abs) -> Some (compile_func_rec ctxt abs var t)

(* functions *)

and compile_func ctxt abs : Lower.func_loc =
  let func_loc, def = compile_func_staged ctxt Ast.VariableMap.empty abs None in
  def ctxt;
  func_loc

and compile_func_rec ctxt (e : Typed.abstraction) rec_var f_ty : Lower.func_loc
    =
  let recs = Ast.VariableMap.add rec_var f_ty Ast.VariableMap.empty in
  let func_loc, def = compile_func_staged ctxt recs e (Some rec_var) in
  def ctxt;
  func_loc

and flat_abstraction ps (abs : Typed.abstraction) =
  match abs.Source.it with p, comp -> flat_computation (p :: ps) comp

and flat_computation ps comp =
  match comp.Source.it with
  | Typed.Return { it = Typed.Lambda abs; _ } -> flat_abstraction ps abs
  | _ -> (ps, comp)

and compile_func_staged ctxt (rec_x : Ast.ty Ast.VariableMap.t)
    (abs : Typed.abstraction) (name : Ast.variable option) : Lower.func_loc * _
    =
  let emit ctxt = List.iter (emit_instr ctxt) in
  let temporary = Typed.(free_case abs -- rec_x) in
  let vars = filter_vars ctxt temporary in
  let ps, comp = flat_abstraction [] abs in
  let fn, def_func = emit_func_deferred ctxt in
  let envflds = Lower.lower_clos_env ctxt vars in
  let ps = List.rev ps in
  let arity = List.length ps in
  let _code, closN, closNenv = Lower.lower_clos_type ctxt arity envflds in
  let def ctxt =
    let argts, argv_opt = Lower.lower_param_types ctxt arity in
    def_func ctxt
      (RefT (NoNull, VarHT (StatX closN)) :: argts)
      [ RefT Lower.absref ]
      (fun ctxt _ ->
        let ctxt = Lower.enter_scope ctxt LocalScope in
        let _, env = Lower.current_scope ctxt in
        let clos = emit_param ctxt in
        let args = List.map (fun _ -> emit_param ctxt) argts in
        let arg0 = List.hd args + 1 in
        compile_load_env ctxt clos closNenv vars envflds;
        (match name with
        | None -> ()
        | Some x ->
            let clos_local =
              Emit.emit_local ctxt
                { ltype = RefT (NoNull, VarHT (StatX closN)) }
            in
            compile_alloc_clos ctxt fn (List.length ps) vars closNenv;
            emit ctxt [ LocalSet clos_local ];
            env :=
              Env.extend_val !env x
                (make_phrase
                   ( Lower.LocalLoc clos_local,
                     Some
                       Lower.
                         {
                           funcidx = fn;
                           typeidx = closN;
                           arity = List.length ps;
                         } )));
        let partial = List.exists (fun p -> classify_pat p = PartialPat) ps in
        (if not partial then
           List.iteri
             (fun i pI ->
               match classify_pat pI with
               | IrrelevantPat -> ()
               | TotalPat ->
                   compile_load_arg ctxt i arg0 argv_opt;
                   compile_coerce ctxt Lower.arg_rep (Lower.pat_rep ()) (et pI);
                   compile_pattern ctxt (-1) pI None
               | PartialPat -> assert false)
             ps
         else
           let block bt es = Wasm.Block (bt, es) in
           emit_block ctxt block (ValBlockType None) (fun ctxt ->
               emit_block ctxt block (ValBlockType None) (fun ctxt ->
                   List.iteri
                     (fun i pI ->
                       match classify_pat pI with
                       | IrrelevantPat -> ()
                       | TotalPat ->
                           compile_load_arg ctxt i arg0 argv_opt;
                           compile_coerce ctxt Lower.arg_rep (Lower.pat_rep ())
                             (et pI);
                           compile_pattern ctxt (-1) pI None
                       | PartialPat ->
                           compile_load_arg ctxt i arg0 argv_opt;
                           compile_coerce ctxt Lower.arg_rep (Lower.pat_rep ())
                             (et pI);
                           compile_pattern ctxt 0 pI None)
                     ps;
                   emit ctxt [ Br 1 ]);
               emit ctxt [ Unreachable ]));
        ignore (compile_computation ctxt comp Lower.arg_rep));
    compile_alloc_clos ctxt fn (List.length ps) vars closNenv
  in
  (Lower.{ funcidx = fn; typeidx = closN; arity = List.length ps }, def)

and compile_computation ctxt comp dst =
  let emit ctxt = List.iter (emit_instr ctxt) in
  match it comp with
  | Typed.Return exp -> compile_expression ctxt exp dst
  | Do (compute, { it = pat, cont; _ }) -> (
      match classify_pat pat with
      | IrrelevantPat ->
          ignore (compile_computation ctxt compute Lower.rigid_rep);
          emit ctxt [ Drop ];
          compile_computation ctxt cont dst
      | TotalPat ->
          let ctxt' = Lower.enter_scope ctxt LocalScope in
          let funcloc_opt =
            compile_computation ctxt' compute (Lower.pat_rep ())
          in
          compile_pattern ctxt' (-1) pat funcloc_opt;
          compile_computation ctxt' cont dst
      | PartialPat -> failwith "let shouldn't have partial patterns")
  | Match (exp, []) ->
      ignore (compile_expression ctxt exp (Lower.pat_rep ()));
      emit ctxt [ Unreachable ];
      None
  | Match (exp, abs_ls) ->
      let t1 = et exp in
      let ty = et comp in
      let tmp =
        emit_local ctxt
          { ltype = Lower.lower_value_type ctxt (Lower.tmp_rep ()) t1 }
      in
      let block bt es = Wasm.Block (bt, es) in
      ignore (compile_expression ctxt exp (Lower.tmp_rep ()));
      emit ctxt [ LocalSet tmp ];
      let bt = Lower.lower_block_type ctxt dst ty in
      emit_block ctxt block bt (fun ctxt ->
          let ends_with_partial =
            List.fold_left
              (fun _ { it = pI, cI; _ } ->
                match classify_pat pI with
                | IrrelevantPat ->
                    ignore (compile_computation ctxt cI dst);
                    emit ctxt [ Br 0 ];
                    false
                | TotalPat ->
                    let ctxt = Lower.enter_scope ctxt LocalScope in
                    let typ = et pI in
                    emit ctxt [ LocalGet tmp ];
                    compile_coerce ctxt (Lower.tmp_rep ()) (Lower.pat_rep ())
                      typ;
                    compile_pattern ctxt (-1) pI None;
                    ignore (compile_computation ctxt cI dst);
                    emit ctxt [ Br 0 ];
                    false
                | PartialPat ->
                    let ctxt = Lower.enter_scope ctxt LocalScope in
                    emit_block ctxt block (ValBlockType None) (fun ctxt ->
                        emit ctxt [ LocalGet tmp ];
                        compile_coerce ctxt (Lower.tmp_rep ())
                          (Lower.pat_rep ()) t1;
                        compile_pattern ctxt 0 pI None;
                        ignore (compile_computation ctxt cI dst);
                        emit ctxt [ Br 1 ]);
                    true)
              true (List.rev abs_ls)
          in
          if ends_with_partial then emit ctxt [ Unreachable ]);
      None
  | Apply (exp1, exp2) -> (
      let ty = et comp in
      match exp1.it with
      | Var x ->
          (match find_val_var_opt ctxt x ctxt.ext.envs with
          | Some _ ->
              ignore (compile_expression ctxt exp1 Lower.arg_rep);
              ignore (compile_expression ctxt exp2 Lower.arg_rep);
              emit ctxt [ Call (compile_func_apply 1 ctxt) ];
              compile_coerce ctxt Lower.arg_rep dst ty
          | None -> (
              match Source.it exp2 with
              | Tuple [ binexp1; binexp2 ] ->
                  ignore (compile_expression ctxt binexp1 Lower.rigid_rep);
                  ignore (compile_expression ctxt binexp2 Lower.rigid_rep);
                  Primitives.simple_primitive_function ctxt
                    (Source.it (find_prim ctxt x ctxt.ext.envs));
                  compile_coerce ctxt Lower.rigid_rep dst ty
              | Var _ ->
                  ignore (compile_expression ctxt exp2 Lower.rigid_rep);
                  Primitives.simple_primitive_function ctxt
                    (Source.it (find_prim ctxt x ctxt.ext.envs));
                  compile_coerce ctxt Lower.rigid_rep dst ty
              | _ ->
                  Typed.print_expression exp2 Format.str_formatter;
                  print_endline (Format.flush_str_formatter ());
                  failwith "how"));
          None
      | Lambda _ ->
          (match compile_expression ctxt exp1 Lower.rigid_rep with
          | Some { funcidx; arity; _ } when arity = 1 ->
              ignore (compile_expression ctxt exp2 Lower.arg_rep);
              emit ctxt [ Call funcidx ];
              compile_coerce ctxt Lower.arg_rep dst ty
          | Some _ ->
              ignore (compile_expression ctxt exp2 Lower.arg_rep);
              emit ctxt [ Call (compile_func_apply 1 ctxt) ];
              compile_coerce ctxt Lower.arg_rep dst ty
          | None -> failwith "compiling a lambda should return a func_loc");
          None
      | _ -> assert false)

let compile_ty_defs ctxt ty_defs =
  let _, env = Lower.current_scope ctxt in
  let compile_ty_def name ty_def =
    match ty_def with
    | _, Ast.TyInline t ->
        let _ = Lower.lower_inline_type ctxt t in
        env := Env.extend_typ !env name (make_phrase [])
    | _, Ast.TySum t_ls ->
        let x =
          (List.mapi (fun i (lbl, ty_opt) ->
               let tyidx = Lower.lower_sum_type ctxt ty_opt in
               Lower.(lbl, { tag = i; typeidx = tyidx })))
            t_ls
        in
        env := Env.extend_typ !env name (make_phrase x)
  in
  Ast.TyNameMap.iter compile_ty_def ty_defs

let rec compile_command ctxt cmd dst =
  ignore dst;
  match cmd with
  | Typed.TyDef _ -> failwith "types should already be processed"
  | TopLet (name, exp) -> (
      match exp.it with
      | Typed.Lambda _ | RecLambda _ ->
          let func_loc_opt = compile_expression ctxt exp (Lower.local_rep ()) in
          compile_val_var_bind ctxt name (et exp) (Lower.global_rep ())
            func_loc_opt
      | _ ->
          let func_loc_opt = compile_expression ctxt exp (Lower.local_rep ()) in
          compile_val_var_bind ctxt name (et exp) (Lower.global_rep ())
            func_loc_opt)
  | TopDo cmp ->
      let _, env = Lower.current_scope ctxt in
      let ret_ty =
        Lower.lower_value_type ctxt Lower.rigid_rep (Source.et cmp)
      in
      let idx =
        Emit.emit_func ctxt [] [ ret_ty ] (fun ctxt _ ->
            ignore (compile_computation ctxt cmp Lower.rigid_rep))
      in
      let name = "run_" ^ string_of_int !env.runs in
      emit_func_export ctxt name idx;
      env := Env.update_runs !env

and compile_commands ctxt ds dst : unit =
  match ds with
  | [] -> compile_coerce ctxt Lower.unit_rep dst (Ast.TyTuple [])
  | [ d ] -> compile_command ctxt d dst
  | d :: ds' ->
      compile_command ctxt d Lower.DropRep;
      compile_commands ctxt ds' dst

and compile_prims ctxt prims =
  let _, env = Lower.current_scope ctxt in
  List.iter
    (fun (var, prim) -> env := Env.extend_prim !env var (make_phrase prim))
    prims

(* TODO add start function *)
and compile_prog (cmds : comp_state)
    (typs : (Ast.ty_param list * Ast.ty_def) Ast.TyNameMap.t) : string =
  let ctxt = Lower.enter_scope (Lower.make_ctxt ()) GlobalScope in
  (*add primitves*)
  compile_prims ctxt cmds.prims;
  (*compile types*)
  compile_ty_defs ctxt typs;
  (* Compile declarations directly *)
  let start_idx =
    emit_func ctxt [] [] (fun ctxt _ ->
        compile_commands ctxt cmds.cmds Lower.rigid_rep)
  in
  emit_start ctxt start_idx;
  Emit.gen_module ctxt |> Arrange.module_ |> Sexpr.to_string 64
