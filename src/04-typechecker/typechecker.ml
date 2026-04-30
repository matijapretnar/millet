open Utils
module Ast = Language.Ast
module Const = Language.Const
module TypedAst = TypedAst

let make_phrase x t = Source.{ it = x; et = Some t }

type state = {
  variables : (Ast.ty_param list * Ast.ty) Ast.VariableMap.t;
  type_definitions : (Ast.ty_param list * Ast.ty_def) Ast.TyNameMap.t;
}

let initial_state =
  {
    variables = Ast.VariableMap.empty;
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
  let subst = refreshing_subst params in
  let args = List.map (fun param -> Ast.TyParamMap.find param subst) params
  and ty' = Option.map (Ast.substitute_ty subst) ty in
  (ty', Ast.TyApply (ty_name, args))

let rec infer_pattern state = function
  | Ast.PVar x ->
      let ty = fresh_ty () in
      let pat = TypedAst.make_pattern (TypedAst.PVar x) ty in
      (ty, [ (x, ty) ], [], pat)
  | Ast.PAs (pat, x) ->
      let ty, vars, eqs, pat' = infer_pattern state pat in
      let phrase_pat = TypedAst.make_pattern (TypedAst.PAs (pat', x)) ty in
      (ty, (x, ty) :: vars, eqs, phrase_pat)
  | Ast.PAnnotated (pat, ty) ->
      let ty', vars, eqs, pat' = infer_pattern state pat in
      let phrase_pat = TypedAst.make_pattern (PAnnotated (pat', ty)) ty in
      (ty, vars, (ty, ty') :: eqs, phrase_pat)
  | Ast.PConst c as x ->
      let pat = TypedAst.make_const_pat x in
      (Ast.TyConst (Const.infer_ty c), [], [], pat)
  | Ast.PNonbinding ->
      let ty = fresh_ty () in
      let pat = TypedAst.make_pattern PNonbinding ty in
      (ty, [], [], pat)
  | Ast.PTuple pats ->
      let fold pat (tys, vars, eqs, pats) =
        let ty', vars', eqs', pat' = infer_pattern state pat in
        (ty' :: tys, vars' @ vars, eqs' @ eqs, pat' :: pats)
      in
      let tys, vars, eqs, pats = List.fold_right fold pats ([], [], [], []) in
      let phrase_pat = TypedAst.make_pattern (PTuple pats) (Ast.TyTuple tys) in
      (Ast.TyTuple tys, vars, eqs, phrase_pat)
  | Ast.PVariant (lbl, pat) -> (
      let ty_in, ty_out = infer_variant state lbl in
      match (ty_in, pat) with
      | None, None ->
          let phrase_pat =
            TypedAst.make_pattern (PVariant (lbl, None)) ty_out
          in
          (ty_out, [], [], phrase_pat)
      | Some ty_in, Some pat ->
          let ty, vars, eqs, pat' = infer_pattern state pat in
          let phrase_pat =
            TypedAst.make_pattern (PVariant (lbl, Some pat')) ty_out
          in
          (ty_out, vars, (ty_in, ty) :: eqs, phrase_pat)
      | None, Some _ | Some _, None ->
          Error.typing "Variant optional argument mismatch")

let rec infer_expression state = function
  | Ast.Var x ->
      let params, ty = Ast.VariableMap.find x state.variables in
      let subst = refreshing_subst params in
      let ty' = Ast.substitute_ty subst ty in
      let expr = TypedAst.make_expression (Var x) ty' in
      (ty', [], expr)
  | Ast.Const c as x ->
      let expr = TypedAst.make_const_expr x in
      (Ast.TyConst (Const.infer_ty c), [], expr)
  | Ast.Annotated (expr, ty) ->
      let ty', eqs, expr' = infer_expression state expr in
      let phrase_expr = TypedAst.make_expression (Annotated (expr', ty)) ty in
      (ty, (ty, ty') :: eqs, phrase_expr)
  | Ast.Tuple exprs ->
      let fold expr (tys, eqs, exprs) =
        let ty', eqs', expr' = infer_expression state expr in
        (ty' :: tys, eqs' @ eqs, expr' :: exprs)
      in
      let tys, eqs, exprs = List.fold_right fold exprs ([], [], []) in
      let phrase_expr =
        TypedAst.make_expression (Tuple exprs) (Ast.TyTuple tys)
      in
      (Ast.TyTuple tys, eqs, phrase_expr)
  | Ast.Lambda abs ->
      let ty, ty', eqs, abs = infer_abstraction state abs in
      let expr = TypedAst.make_expression (Lambda abs) (TyArrow (ty, ty')) in
      (Ast.TyArrow (ty, ty'), eqs, expr)
  | Ast.RecLambda (f, abs) ->
      let f_ty = fresh_ty () in
      let state' = extend_variables state [ (f, f_ty) ] in
      let ty, ty', eqs, abs = infer_abstraction state' abs in
      let out_ty = Ast.TyArrow (ty, ty') in
      let expr =
        TypedAst.make_expression (RecLambda (f, abs)) (TyArrow (f_ty, out_ty))
      in
      (out_ty, (f_ty, out_ty) :: eqs, expr)
  | Ast.Variant (lbl, expr) -> (
      let ty_in, ty_out = infer_variant state lbl in
      match (ty_in, expr) with
      | None, None ->
          let expr = TypedAst.make_expression (Variant (lbl, None)) ty_out in
          (ty_out, [], expr)
      | Some ty_in, Some expr ->
          let ty, eqs, expr' = infer_expression state expr in
          let phrase_expr =
            TypedAst.make_expression (Variant (lbl, Some expr')) ty_out
          in
          (ty_out, (ty_in, ty) :: eqs, phrase_expr)
      | None, Some _ | Some _, None ->
          Error.typing "Variant optional argument mismatch")

and infer_computation state = function
  | Ast.Return expr ->
      let ty, eqs, expr' = infer_expression state expr in
      let comp = TypedAst.make_computation (Return expr') ty in
      (ty, eqs, comp)
  | Ast.Do (comp1, comp2) ->
      let ty1, eqs1, comp1' = infer_computation state comp1 in
      let ty1', ty2, eqs2, abs' = infer_abstraction state comp2 in
      let comp = TypedAst.make_computation (Do (comp1', abs')) ty2 in
      (ty2, ((ty1, ty1') :: eqs1) @ eqs2, comp)
  | Ast.Apply (e1, e2) ->
      let t1, eqs1, expr1 = infer_expression state e1
      and t2, eqs2, expr2 = infer_expression state e2
      and a = fresh_ty () in
      let comp = TypedAst.make_computation (Apply (expr1, expr2)) a in
      (a, ((t1, Ast.TyArrow (t2, a)) :: eqs1) @ eqs2, comp)
  | Ast.Match (e, cases) ->
      let ty1, eqs, expr = infer_expression state e and ty2 = fresh_ty () in
      let fold (eqs, abstractions) abs =
        let ty1', ty2', eqs', abs' = infer_abstraction state abs in
        let updated_eqs = ((ty1, ty1') :: (ty2, ty2') :: eqs') @ eqs in
        let updated_abstractions = abs' :: abstractions in
        (updated_eqs, updated_abstractions)
      in
      let eqs, abstractions = List.fold_left fold (eqs, []) cases in
      let comp = TypedAst.make_computation (Match (expr, abstractions)) ty2 in
      (ty2, eqs, comp)

and infer_abstraction state (pat, comp) =
  let ty, vars, eqs, pat' = infer_pattern state pat in
  let state' = extend_variables state vars in
  let ty', eqs', comp' = infer_computation state' comp in
  let abs = TypedAst.make_abstraction (pat', comp') (TyArrow (ty, ty')) in
  (ty, ty', eqs @ eqs', abs)

let subst_equations sbst =
  let subst_equation (t1, t2) =
    (Ast.substitute_ty sbst t1, Ast.substitute_ty sbst t2)
  in
  List.map subst_equation

let add_subst a t sbst = Ast.TyParamMap.add a (Ast.substitute_ty sbst t) sbst

let rec occurs a = function
  | Ast.TyParam a' -> a = a'
  | Ast.TyConst _ -> false
  | Ast.TyArrow (ty1, ty2) -> occurs a ty1 || occurs a ty2
  | Ast.TyApply (_, tys) -> List.exists (occurs a) tys
  | Ast.TyTuple tys -> List.exists (occurs a) tys

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

let rec unify state = function
  | [] -> Ast.TyParamMap.empty
  | (t1, t2) :: eqs when t1 = t2 -> unify state eqs
  | (Ast.TyApply (ty_name1, args1), Ast.TyApply (ty_name2, args2)) :: eqs
    when ty_name1 = ty_name2 ->
      unify state (List.combine args1 args2 @ eqs)
  | (Ast.TyApply (ty_name, args), ty) :: eqs
    when is_transparent_type state ty_name ->
      unify state ((unfold state ty_name args, ty) :: eqs)
  | (ty, Ast.TyApply (ty_name, args)) :: eqs
    when is_transparent_type state ty_name ->
      unify state ((ty, unfold state ty_name args) :: eqs)
  | (Ast.TyTuple tys1, Ast.TyTuple tys2) :: eqs
    when List.length tys1 = List.length tys2 ->
      unify state (List.combine tys1 tys2 @ eqs)
  | (Ast.TyArrow (t1, t1'), Ast.TyArrow (t2, t2')) :: eqs ->
      unify state ((t1, t2) :: (t1', t2') :: eqs)
  | (Ast.TyParam a, t) :: eqs when not (occurs a t) ->
      add_subst a t
        (unify state (subst_equations (Ast.TyParamMap.singleton a t) eqs))
  | (t, Ast.TyParam a) :: eqs when not (occurs a t) ->
      add_subst a t
        (unify state (subst_equations (Ast.TyParamMap.singleton a t) eqs))
  | (t1, t2) :: _ ->
      let print_param = Ast.new_print_param () in
      Error.typing "Cannot unify %t = %t"
        (Ast.print_ty print_param t1)
        (Ast.print_ty print_param t2)

let update_et_value phrase sbst =
  match phrase.Source.et with
  | None -> ()
  | Some x -> Source.update_et_value phrase (Ast.substitute_ty sbst x)

let rec update_pattern sbst pat =
  update_et_value pat sbst;
  match pat.Source.it with
  | TypedAst.PAnnotated (p, _) -> update_pattern sbst p
  | PAs (p, _) -> update_pattern sbst p
  | PTuple ps -> List.iter (update_pattern sbst) ps
  | PVariant (_, p) -> (
      match p with None -> () | Some p -> update_pattern sbst p)
  | _ -> ()

and update_expression sbst expr =
  update_et_value expr sbst;
  match expr.Source.it with
  | TypedAst.Annotated (e, _) -> update_expression sbst e
  | Tuple es -> List.iter (update_expression sbst) es
  | Variant (_, e) -> (
      match e with None -> () | Some e -> update_expression sbst e)
  | Lambda abs -> update_abstraction sbst abs
  | RecLambda (_, abs) -> update_abstraction sbst abs
  | _ -> ()

and update_computation sbst (comp : TypedAst.computation) =
  update_et_value comp sbst;
  match comp.Source.it with
  | TypedAst.Match (exp, abs) ->
      update_expression sbst exp;
      List.iter (update_abstraction sbst) abs
  | Do (comp1, comp2) ->
      update_computation sbst comp1;
      update_abstraction sbst comp2
  | Apply (exp1, exp2) ->
      update_expression sbst exp1;
      update_expression sbst exp2
  | Return x -> update_expression sbst x

and update_abstraction sbst (abs : TypedAst.abstraction) =
  match abs.Source.it with
  | pat, comp ->
      update_pattern sbst pat;
      update_computation sbst comp;
      update_et_value abs sbst

let infer state e =
  let t, eqs, comp = infer_computation state e in
  let sbst = unify state eqs in
  let t' = Ast.substitute_ty sbst t in
  update_computation sbst comp;
  (t', comp)

let add_external_function x ty_sch state =
  { state with variables = Ast.VariableMap.add x ty_sch state.variables }

let add_top_definition state x expr =
  let ty, eqs, expr' = infer_expression state expr in
  let subst = unify state eqs in
  let ty' = Ast.substitute_ty subst ty in
  let free_vars = Ast.free_vars ty' |> Ast.TyParamSet.elements in
  let ty_sch = (free_vars, ty') in
  update_expression subst expr';
  (add_external_function x ty_sch state, TypedAst.TopLet (x, expr'))

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

let rec eq t1 t2 =
  t1 == t2
  ||
  match (t1, t2) with
  | Ast.TyTuple tys1, Ast.TyTuple tys2 ->
      List.length tys1 = List.length tys2 && List.for_all2 eq tys1 tys2
  | Ast.TyArrow (from1, to1), Ast.TyArrow (from2, to2) ->
      eq from1 from2 && eq to1 to2
  | Ast.TyApply (name1, params1), Ast.TyApply (name2, params2) ->
      List.length params1 = List.length params2
      && List.for_all2 eq params1 params2
      && name1 == name2
  | Ast.TyConst ty1, Ast.TyConst ty2 -> ty1 == ty2
  | ty1, ty2 -> ty1 = ty2
