open Utils
module Ast = Language.Ast
module Const = Language.Const

type state = {
  variables : (Ast.ty_param list * Ast.ty) Ast.VariableMap.t;
  type_definitions : (Ast.ty_param list * Ast.ty_def) Ast.TyNameMap.t;
  rigid_params : Ast.TyParamSet.t;
      (** Params of the checked top-level definition; unification cannot bind
          them. All other params are unification variables. *)
}

let initial_state =
  {
    variables = Ast.VariableMap.empty;
    rigid_params = Ast.TyParamSet.empty;
    type_definitions =
      (Ast.TyNameMap.empty
      |> Ast.TyNameMap.add Ast.bool_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.BooleanTy))
      |> Ast.TyNameMap.add Ast.int_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.IntegerTy))
      |> Ast.TyNameMap.add Ast.unit_ty_name ([], Ast.TyInline (Ast.TyTuple []))
      |> Ast.TyNameMap.add Ast.string_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.StringTy))
      |> Ast.TyNameMap.add Ast.float_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.FloatTy))
      |> Ast.TyNameMap.add Ast.empty_ty_name ([], Ast.TySum [])
      |>
      let a = Ast.TyParam.fresh "list" in
      Ast.TyNameMap.add Ast.list_ty_name
        ( [ a ],
          Ast.TySum
            [
              (Ast.nil_label, None);
              ( Ast.cons_label,
                Some
                  (Ast.TyTuple
                     [
                       Ast.TyParam a;
                       Ast.TyApply (Ast.list_ty_name, [ Ast.TyParam a ]);
                     ]) );
            ] ));
  }

let rec check_ty state = function
  | Ast.TyConst _ -> ()
  | TyApply (ty_name, tys) ->
      let params, _ = Ast.TyNameMap.find ty_name state.type_definitions in
      let expected, actual = (List.length params, List.length tys) in
      if expected <> actual then
        Error.typing "Type %t expects %d arguments but got %d."
          (Ast.TyName.print ty_name) expected actual
      else List.iter (check_ty state) tys
  | TyParam _ -> ()
  | TyArrow (ty1, ty2) ->
      check_ty state ty1;
      check_ty state ty2
  | TyTuple tys -> List.iter (check_ty state) tys

let check_variant state (_label, arg_ty) =
  match arg_ty with None -> () | Some ty -> check_ty state ty

let check_ty_def state = function
  | Ast.TySum defs -> List.iter (check_variant state) defs
  | Ast.TyInline ty -> check_ty state ty

let fresh_ty () =
  let a = Ast.TyParam.fresh "ty" in
  Ast.TyParam a

let extend_variables state vars =
  List.fold_left
    (fun state (x, ty) ->
      { state with variables = Ast.VariableMap.add x ([], ty) state.variables })
    state vars

let refreshing_subst params =
  List.fold_left
    (fun subst param ->
      let ty = fresh_ty () in
      Ast.TyParamMap.add param ty subst)
    Ast.TyParamMap.empty params

let rec resolve subst = function
  | Ast.TyParam a as ty -> (
      match Ast.TyParamMap.find_opt a !subst with
      | Some ty' -> resolve subst ty'
      | None -> ty)
  | Ast.TyConst _ as ty -> ty
  | Ast.TyArrow (ty1, ty2) -> Ast.TyArrow (resolve subst ty1, resolve subst ty2)
  | Ast.TyTuple tys -> Ast.TyTuple (List.map (resolve subst) tys)
  | Ast.TyApply (ty_name, tys) ->
      Ast.TyApply (ty_name, List.map (resolve subst) tys)

let rec occurs subst a ty =
  match resolve subst ty with
  | Ast.TyParam a' -> a = a'
  | Ast.TyConst _ -> false
  | Ast.TyArrow (ty1, ty2) -> occurs subst a ty1 || occurs subst a ty2
  | Ast.TyApply (_, tys) -> List.exists (occurs subst a) tys
  | Ast.TyTuple tys -> List.exists (occurs subst a) tys

let is_transparent_type state ty_name =
  match Ast.TyNameMap.find ty_name state.type_definitions with
  | _, Ast.TySum _ -> false
  | _, Ast.TyInline _ -> true

let unfold state ty_name args =
  match Ast.TyNameMap.find ty_name state.type_definitions with
  | _, Ast.TySum _ -> assert false
  | params, Ast.TyInline ty ->
      let subst =
        List.combine params args |> List.to_seq |> Ast.TyParamMap.of_seq
      in
      Ast.substitute_ty subst ty

let bind subst a t =
  if occurs subst a t then
    Error.typing
      "This expression has a type that would require an infinite type"
  else subst := Ast.TyParamMap.add a t !subst

let rec unify state subst t1 t2 =
  let t1 = resolve subst t1 and t2 = resolve subst t2 in
  match (t1, t2) with
  | t1, t2 when t1 = t2 -> ()
  | Ast.TyParam a, t when not (Ast.TyParamSet.mem a state.rigid_params) ->
      bind subst a t
  | t, Ast.TyParam a when not (Ast.TyParamSet.mem a state.rigid_params) ->
      bind subst a t
  | Ast.TyApply (ty_name1, args1), Ast.TyApply (ty_name2, args2)
    when ty_name1 = ty_name2 ->
      List.iter2 (unify state subst) args1 args2
  | Ast.TyApply (ty_name, args), t when is_transparent_type state ty_name ->
      unify state subst (unfold state ty_name args) t
  | t, Ast.TyApply (ty_name, args) when is_transparent_type state ty_name ->
      unify state subst t (unfold state ty_name args)
  | Ast.TyTuple tys1, Ast.TyTuple tys2 when List.length tys1 = List.length tys2
    ->
      List.iter2 (unify state subst) tys1 tys2
  | Ast.TyArrow (t1, t1'), Ast.TyArrow (t2, t2') ->
      unify state subst t1 t2;
      unify state subst t1' t2'
  | t1, t2 ->
      let print_param = Ast.new_print_param () in
      Error.typing "Cannot unify %t = %t"
        (Ast.print_ty print_param t1)
        (Ast.print_ty print_param t2)

let infer_variant state lbl =
  let rec find = function
    | [] -> assert false
    | (_, (_, Ast.TyInline _)) :: ty_defs -> find ty_defs
    | (ty_name, (params, Ast.TySum variants)) :: ty_defs -> (
        match List.assoc_opt lbl variants with
        | None -> find ty_defs
        | Some ty -> (ty_name, params, ty))
  in
  let ty_name, params, ty =
    find (Ast.TyNameMap.bindings state.type_definitions)
  in
  let inst_subst = refreshing_subst params in
  let args = List.map (fun param -> Ast.TyParamMap.find param inst_subst) params
  and ty' = Option.map (Ast.substitute_ty inst_subst) ty in
  (ty', Ast.TyApply (ty_name, args))

let rec check_pattern state subst pat ty =
  match pat with
  | Ast.PVar x -> extend_variables state [ (x, ty) ]
  | Ast.PAs (pat, x) ->
      let state' = check_pattern state subst pat ty in
      extend_variables state' [ (x, ty) ]
  | Ast.PAnnotated (pat, ty') ->
      unify state subst ty ty';
      check_pattern state subst pat ty
  | Ast.PConst c ->
      unify state subst ty (Ast.TyConst (Const.infer_ty c));
      state
  | Ast.PNonbinding -> state
  | Ast.PTuple pats -> (
      match resolve subst ty with
      | Ast.TyTuple tys when List.length pats = List.length tys ->
          List.fold_left2
            (fun state pat ty -> check_pattern state subst pat ty)
            state pats tys
      | ty' ->
          let print_param = Ast.new_print_param () in
          Error.typing "Expected a tuple pattern of type %t, got a %d-tuple"
            (Ast.print_ty print_param ty')
            (List.length pats))
  | Ast.PVariant (lbl, pat_opt) -> (
      let ty_in, ty_out = infer_variant state lbl in
      unify state subst ty ty_out;
      match (ty_in, pat_opt) with
      | None, None -> state
      | Some ty_in, Some pat -> check_pattern state subst pat ty_in
      | None, Some _ | Some _, None ->
          Error.typing "Variant optional argument mismatch")

let rec print_pattern pat ppf =
  match pat with
  | Ast.PVar x -> Ast.Variable.print x ppf
  | Ast.PAs (pat, x) ->
      Format.fprintf ppf "%t as %t" (print_pattern pat) (Ast.Variable.print x)
  | Ast.PAnnotated (pat, _) -> print_pattern pat ppf
  | Ast.PConst _ -> Format.fprintf ppf "<constant>"
  | Ast.PNonbinding -> Format.fprintf ppf "_"
  | Ast.PTuple pats ->
      Format.fprintf ppf "(%t)" (fun ppf ->
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
            (fun ppf pat -> print_pattern pat ppf)
            ppf pats)
  | Ast.PVariant (lbl, None) -> Ast.Label.print lbl ppf
  | Ast.PVariant (lbl, Some pat) ->
      Format.fprintf ppf "%t %t" (Ast.Label.print lbl) (print_pattern pat)

let pattern_annotation = function
  | Ast.PAnnotated (_, ty) -> Some ty
  | _ -> None

let rec synthesize_ty = function
  | Ast.Annotated (_, ty) -> Some ty
  | Ast.Lambda (pat, comp) -> (
      match pattern_annotation pat with
      | None -> None
      | Some ty1 -> (
          match synthesize_computation_ty comp with
          | None -> None
          | Some ty2 -> Some (Ast.TyArrow (ty1, ty2))))
  | _ -> None

and synthesize_computation_ty = function
  | Ast.Return expr -> synthesize_ty expr
  | Ast.Do (_, (_, comp2)) -> synthesize_computation_ty comp2
  | _ -> None

(* ------------------------------------------------------------------ *)
(* Expressions. *)
(* ------------------------------------------------------------------ *)

let rec infer_expression state subst = function
  | Ast.Var x ->
      let params, ty = Ast.VariableMap.find x state.variables in
      let inst_subst = refreshing_subst params in
      Ast.substitute_ty inst_subst ty
  | Ast.Const c -> Ast.TyConst (Const.infer_ty c)
  | Ast.Annotated (expr, ty) ->
      check_expression state subst expr ty;
      ty
  | Ast.Tuple exprs ->
      Ast.TyTuple (List.map (infer_expression state subst) exprs)
  | Ast.Lambda (pat, comp) -> (
      match synthesize_ty (Ast.Lambda (pat, comp)) with
      | Some (Ast.TyArrow (ty1, ty2) as full_ty) ->
          let state' = check_pattern state subst pat ty1 in
          check_computation state' subst comp ty2;
          full_ty
      | Some _ | None -> (
          match pattern_annotation pat with
          | None ->
              Error.typing
                "Cannot infer the type of a function; its argument pattern %t \
                 needs a type annotation (or it needs to be used where an \
                 expected type is known)"
                (print_pattern pat)
          | Some ty1 ->
              let state' = check_pattern state subst pat ty1 in
              let ty2 = infer_computation state' subst comp in
              Ast.TyArrow (ty1, ty2)))
  | Ast.RecLambda (f, (pat, comp)) -> (
      match synthesize_ty (Ast.Lambda (pat, comp)) with
      | Some (Ast.TyArrow (ty1, ty2) as full_ty) ->
          let state' = extend_variables state [ (f, full_ty) ] in
          let state'' = check_pattern state' subst pat ty1 in
          check_computation state'' subst comp ty2;
          full_ty
      | Some _ | None -> (
          match pattern_annotation pat with
          | None ->
              Error.typing
                "Cannot infer the type of a recursive function (bound as %t); \
                 its argument pattern %t needs a type annotation (or it needs \
                 to be used where an expected type is known)"
                (Ast.Variable.print f) (print_pattern pat)
          | Some ty1 ->
              let f_ty = fresh_ty () in
              let state' = extend_variables state [ (f, f_ty) ] in
              let state'' = check_pattern state' subst pat ty1 in
              let ty2 = infer_computation state'' subst comp in
              let out_ty = Ast.TyArrow (ty1, ty2) in
              unify state subst f_ty out_ty;
              out_ty))
  | Ast.Variant (lbl, expr) -> (
      let ty_in, ty_out = infer_variant state lbl in
      match (ty_in, expr) with
      | None, None -> ty_out
      | Some ty_in, Some expr ->
          check_expression state subst expr ty_in;
          ty_out
      | None, Some _ | Some _, None ->
          Error.typing "Variant optional argument mismatch")

and check_expression state subst expr ty =
  match (expr, resolve subst ty) with
  | Ast.Lambda (pat, comp), Ast.TyArrow (ty1, ty2) ->
      let state' = check_pattern state subst pat ty1 in
      check_computation state' subst comp ty2
  | Ast.Lambda _, ty' ->
      let print_param = Ast.new_print_param () in
      Error.typing "This function is expected to have type %t"
        (Ast.print_ty print_param ty')
  | Ast.RecLambda (f, (pat, comp)), (Ast.TyArrow (ty1, ty2) as ty') ->
      let state' = extend_variables state [ (f, ty') ] in
      let state'' = check_pattern state' subst pat ty1 in
      check_computation state'' subst comp ty2
  | Ast.RecLambda _, ty' ->
      let print_param = Ast.new_print_param () in
      Error.typing "This recursive function is expected to have type %t"
        (Ast.print_ty print_param ty')
  | Ast.Tuple exprs, Ast.TyTuple tys when List.length exprs = List.length tys ->
      List.iter2 (check_expression state subst) exprs tys
  | expr, ty ->
      (* Subsumption: infer, then check the inferred type agrees. *)
      let ty' = infer_expression state subst expr in
      unify state subst ty' ty

(* ------------------------------------------------------------------ *)
(* Computations. *)
(* ------------------------------------------------------------------ *)

and infer_computation state subst = function
  | Ast.Return expr -> infer_expression state subst expr
  | Ast.Do (comp1, (pat, comp2)) ->
      let ty1 = infer_computation state subst comp1 in
      let state' = check_pattern state subst pat ty1 in
      infer_computation state' subst comp2
  | Ast.Apply (e1, e2) -> (
      let ty1 = infer_expression state subst e1 in
      match resolve subst ty1 with
      | Ast.TyArrow (ty_arg, ty_res) ->
          check_expression state subst e2 ty_arg;
          ty_res
      | ty' ->
          let print_param = Ast.new_print_param () in
          Error.typing "This expression of type %t is not a function"
            (Ast.print_ty print_param ty'))
  | Ast.Match (_, []) ->
      Error.typing
        "Cannot infer the type of a match expression with no cases; an \
         annotation is required"
  | Ast.Match (expr, (pat1, comp1) :: cases) ->
      let ty_scrutinee = infer_expression state subst expr in
      let state1 = check_pattern state subst pat1 ty_scrutinee in
      let ty = infer_computation state1 subst comp1 in
      List.iter
        (fun (pat, comp) ->
          let state' = check_pattern state subst pat ty_scrutinee in
          check_computation state' subst comp ty)
        cases;
      ty

and check_computation state subst comp ty =
  match comp with
  | Ast.Return expr -> check_expression state subst expr ty
  | Ast.Do (comp1, (pat, comp2)) ->
      let ty1 = infer_computation state subst comp1 in
      let state' = check_pattern state subst pat ty1 in
      check_computation state' subst comp2 ty
  | Ast.Match (expr, cases) ->
      let ty_scrutinee = infer_expression state subst expr in
      List.iter
        (fun (pat, comp) ->
          let state' = check_pattern state subst pat ty_scrutinee in
          check_computation state' subst comp ty)
        cases
  | comp ->
      let ty' = infer_computation state subst comp in
      unify state subst ty' ty

(* ------------------------------------------------------------------ *)
(* Entry points required by the user interface. *)
(* ------------------------------------------------------------------ *)

let infer state comp =
  let subst = ref Ast.TyParamMap.empty in
  let ty = infer_computation state subst comp in
  resolve subst ty

let add_external_function x ty_sch state =
  { state with variables = Ast.VariableMap.add x ty_sch state.variables }

let add_top_definition state x ((params, ty) as ty_sch) expr =
  let subst = ref Ast.TyParamMap.empty in
  let state' = { state with rigid_params = Ast.TyParamSet.of_list params } in
  check_expression state' subst expr ty;
  add_external_function x ty_sch state

let add_type_definitions state ty_defs =
  let state' =
    List.fold_left
      (fun state (params, ty_name, ty_def) ->
        {
          state with
          type_definitions =
            Ast.TyNameMap.add ty_name (params, ty_def) state.type_definitions;
        })
      state ty_defs
  in
  List.iter (fun (_, _, ty_def) -> check_ty_def state' ty_def) ty_defs;
  state'

let load_primitive state x prim =
  let ty_sch = Primitives.primitive_type_scheme prim in
  add_external_function x ty_sch state
