(** Desugaring of syntax into the core language. *)

open Utils
module Sugared = Parser.SugaredAst
module Untyped = Language.Ast
module Const = Language.Const
module StringMap = Map.Make (String)

let add_unique ~loc kind str symb string_map =
  StringMap.update str
    (function
      | None -> Some symb
      | Some _ -> Error.syntax ~loc "%s %s defined multiple times." kind str)
    string_map

type state = {
  ty_names : Untyped.ty_name StringMap.t;
  ty_params : Untyped.ty_param StringMap.t;
  variables : Untyped.variable StringMap.t;
  labels : Untyped.label StringMap.t;
}

let initial_state =
  {
    ty_names =
      StringMap.empty
      |> StringMap.add Sugared.bool_ty_name Untyped.bool_ty_name
      |> StringMap.add Sugared.int_ty_name Untyped.int_ty_name
      |> StringMap.add Sugared.unit_ty_name Untyped.unit_ty_name
      |> StringMap.add Sugared.string_ty_name Untyped.string_ty_name
      |> StringMap.add Sugared.float_ty_name Untyped.float_ty_name
      |> StringMap.add Sugared.empty_ty_name Untyped.empty_ty_name
      |> StringMap.add Sugared.list_ty_name Untyped.list_ty_name;
    ty_params = StringMap.empty;
    variables = StringMap.empty;
    labels =
      StringMap.empty
      |> StringMap.add Sugared.nil_label Untyped.nil_label
      |> StringMap.add Sugared.cons_label Untyped.cons_label;
  }

let find_symbol ~loc map name =
  match StringMap.find_opt name map with
  | None -> Error.syntax ~loc "Unknown name --%s--" name
  | Some symbol -> symbol

let lookup_ty_name ~loc state = find_symbol ~loc state.ty_names
let lookup_ty_param ~loc state = find_symbol ~loc state.ty_params
let lookup_variable ~loc state = find_symbol ~loc state.variables
let lookup_label ~loc state = find_symbol ~loc state.labels

let rec strip_annotation ({ Sugared.it = term; at = loc } : Sugared.term) =
  match term with
  | Sugared.Annotated (t, ty) ->
      let inner, tys = strip_annotation t in
      (inner, ty :: tys)
  | _ -> (({ Sugared.it = term; at = loc } : Sugared.term), [])

let rec desugar_ty state { Sugared.it = plain_ty; at = loc } =
  desugar_plain_ty ~loc state plain_ty

and desugar_plain_ty ~loc state = function
  | Sugared.TyApply (ty_name, tys) ->
      let ty_name' = lookup_ty_name ~loc state ty_name in
      let tys' = List.map (desugar_ty state) tys in
      Untyped.TyApply (ty_name', tys')
  | Sugared.TyParam ty_param ->
      let ty_param' = lookup_ty_param ~loc state ty_param in
      Untyped.TyParam ty_param'
  | Sugared.TyArrow (ty1, ty2) ->
      let ty1' = desugar_ty state ty1 in
      let ty2' = desugar_ty state ty2 in
      Untyped.TyArrow (ty1', ty2')
  | Sugared.TyTuple tys ->
      let tys' = List.map (desugar_ty state) tys in
      Untyped.TyTuple tys'
  | Sugared.TyConst c -> Untyped.TyConst c

let rec desugar_pattern state vars { Sugared.it = pat; at = loc } =
  let vars, pat' = desugar_plain_pattern ~loc state vars pat in
  (vars, pat')

and desugar_plain_pattern ~loc state vars = function
  | Sugared.PVar x ->
      let x' = Untyped.Variable.fresh x in
      (StringMap.singleton x x', Untyped.PVar x')
  | Sugared.PAnnotated (pat, ty) ->
      let vars, pat' = desugar_pattern state vars pat
      and ty' = desugar_ty state ty in
      (vars, Untyped.PAnnotated (pat', ty'))
  | Sugared.PAs (pat, x) ->
      let vars, pat' = desugar_pattern state vars pat in
      let x' = Untyped.Variable.fresh x in
      (add_unique ~loc "Variable" x x' vars, Untyped.PAs (pat', x'))
  | Sugared.PTuple ps ->
      let aux p (vars, ps') =
        let vars', p' = desugar_pattern state vars p in
        (StringMap.fold (add_unique ~loc "Variable") vars' vars, p' :: ps')
      in
      let vars, ps' = List.fold_right aux ps (StringMap.empty, []) in
      (vars, Untyped.PTuple ps')
  | Sugared.PVariant (lbl, None) ->
      let lbl' = lookup_label ~loc state lbl in
      (StringMap.empty, Untyped.PVariant (lbl', None))
  | Sugared.PVariant (lbl, Some pat) ->
      let lbl' = lookup_label ~loc state lbl in
      let vars, pat' = desugar_pattern state vars pat in
      (vars, Untyped.PVariant (lbl', Some pat'))
  | Sugared.PConst c -> (StringMap.empty, Untyped.PConst c)
  | Sugared.PNonbinding -> (StringMap.empty, Untyped.PNonbinding)

let add_fresh_variables state vars =
  let aux x x' variables = StringMap.add x x' variables in
  let variables' = StringMap.fold aux vars state.variables in
  { state with variables = variables' }

let rec desugar_expression state { Sugared.it = term; at = loc } =
  let binds, expr = desugar_plain_expression ~loc state term in
  (binds, expr)

and desugar_plain_expression ~loc state = function
  | Sugared.Var x ->
      let x' = lookup_variable ~loc state x in
      ([], Untyped.Var x')
  | Sugared.Const k -> ([], Untyped.Const k)
  | Sugared.Annotated (term, ty) ->
      let binds, expr = desugar_expression state term in
      (binds, Untyped.Annotated (expr, desugar_ty state ty))
  | Sugared.Lambda a ->
      let a' = desugar_abstraction state a in
      ([], Untyped.Lambda a')
  | Sugared.Function cases ->
      let x = Untyped.Variable.fresh "arg" in
      let cases' = List.map (desugar_abstraction state) cases in
      ( [],
        Untyped.Lambda (Untyped.PVar x, Untyped.Match (Untyped.Var x, cases'))
      )
  | Sugared.Tuple ts ->
      let binds, es = desugar_expressions state ts in
      (binds, Untyped.Tuple es)
  | Sugared.Variant (lbl, None) ->
      let lbl' = lookup_label ~loc state lbl in
      ([], Untyped.Variant (lbl', None))
  | Sugared.Variant (lbl, Some term) ->
      let lbl' = lookup_label ~loc state lbl in
      let binds, expr = desugar_expression state term in
      (binds, Untyped.Variant (lbl', Some expr))
  | ( Sugared.Apply _ | Sugared.Match _ | Sugared.Let _ | Sugared.LetRec _
    | Sugared.Conditional _ ) as term ->
      let x = Untyped.Variable.fresh "b" in
      let comp = desugar_computation state { Sugared.it = term; at = loc } in
      let hoist = (Untyped.PVar x, comp) in
      ([ hoist ], Untyped.Var x)

and desugar_computation state { Sugared.it = term; at = loc } =
  let binds, comp = desugar_plain_computation ~loc state term in
  List.fold_right (fun (p, c1) c2 -> Untyped.Do (c1, (p, c2))) binds comp

and desugar_plain_computation ~loc state =
  let if_then_else e c1 c2 =
    let true_p = Untyped.PConst Const.of_true in
    let false_p = Untyped.PConst Const.of_false in
    Untyped.Match (e, [ (true_p, c1); (false_p, c2) ])
  in
  function
  | Sugared.Apply
      ({ it = Sugared.Var "(&&)"; _ }, { it = Sugared.Tuple [ t1; t2 ]; _ }) ->
      let binds1, e1 = desugar_expression state t1 in
      let c1 = desugar_computation state t2 in
      let c2 = Untyped.Return (Untyped.Const (Const.Boolean false)) in
      (binds1, if_then_else e1 c1 c2)
  | Sugared.Apply
      ({ it = Sugared.Var "(||)"; _ }, { it = Sugared.Tuple [ t1; t2 ]; _ }) ->
      let binds1, e1 = desugar_expression state t1 in
      let c1 = Untyped.Return (Untyped.Const (Const.Boolean true)) in
      let c2 = desugar_computation state t2 in
      (binds1, if_then_else e1 c1 c2)
  | Sugared.Apply (t1, t2) ->
      let binds1, e1 = desugar_expression state t1 in
      let binds2, e2 = desugar_expression state t2 in
      (binds1 @ binds2, Untyped.Apply (e1, e2))
  | Sugared.Match (t, cs) ->
      let binds, e = desugar_expression state t in
      let cs' = List.map (desugar_abstraction state) cs in
      (binds, Untyped.Match (e, cs'))
  | Sugared.Conditional (t, t1, t2) ->
      let binds, e = desugar_expression state t in
      let c1 = desugar_computation state t1 in
      let c2 = desugar_computation state t2 in
      (binds, if_then_else e c1 c2)
  | Sugared.Let (pat, term1, term2) ->
      let c1 = desugar_computation state term1 in
      let c2 = desugar_abstraction state (pat, term2) in
      ([], Untyped.Do (c1, c2))
  | Sugared.LetRec (x, term1, term2) ->
      let state', f, comp1 = desugar_let_rec_def state (x, term1) in
      let c = desugar_computation state' term2 in
      ([], Untyped.Do (Untyped.Return comp1, (Untyped.PVar f, c)))
  (* The remaining cases are expressions, which we list explicitly to catch any
     future changeSugared. *)
  | ( Sugared.Var _ | Sugared.Const _ | Sugared.Annotated _ | Sugared.Tuple _
    | Sugared.Variant _ | Sugared.Lambda _ | Sugared.Function _ ) as term ->
      let binds, expr = desugar_expression state { it = term; at = loc } in
      (binds, Untyped.Return expr)

and desugar_abstraction state (pat, term) =
  let vars, pat' = desugar_pattern state StringMap.empty pat in
  let state' = add_fresh_variables state vars in
  let comp = desugar_computation state' term in
  (pat', comp)

and desugar_let_rec_def state (f, term) =
  let term, annotations = strip_annotation term in

  let f' = Untyped.Variable.fresh f in
  let state' = add_fresh_variables state (StringMap.singleton f f') in
  let abs' =
    match term.Sugared.it with
    | Sugared.Lambda a -> desugar_abstraction state' a
    | Sugared.Function cs ->
        let x = Untyped.Variable.fresh "rf" in
        let cs = List.map (desugar_abstraction state') cs in
        let new_match = Untyped.Match (Untyped.Var x, cs) in
        (Untyped.PVar x, new_match)
    | _ ->
        Error.syntax ~loc:term.at
          "This kind of expression is not allowed in a recursive definition"
  in
  let expr = Untyped.RecLambda (f', abs') in
  let expr =
    List.fold_left
      (fun expr ty -> Untyped.Annotated (expr, desugar_ty state ty))
      expr annotations
  in
  (state', f', expr)

and desugar_expressions state = function
  | [] -> ([], [])
  | t :: ts ->
      let binds, e = desugar_expression state t in
      let ws, es = desugar_expressions state ts in
      (binds @ ws, e :: es)

let desugar_pure_expression state term =
  let binds, expr = desugar_expression state term in
  match binds with
  | [] -> expr
  | _ -> Error.syntax ~loc:term.at "Only pure expressions are allowed"

let add_label ~loc state label label' =
  let labels' = add_unique ~loc "Label" label label' state.labels in
  { state with labels = labels' }

let add_fresh_ty_names ~loc state vars =
  let aux ty_names (x, x') = add_unique ~loc "Type" x x' ty_names in
  let ty_names' = List.fold_left aux state.ty_names vars in
  { state with ty_names = ty_names' }

let add_fresh_ty_params state params =
  let aux ty_params p = StringMap.add p (Untyped.TyParam.fresh p) ty_params in
  let ty_params' = List.fold_left aux state.ty_params params in
  { state with ty_params = ty_params' }

let pattern_annotation { Sugared.it = pat; _ } =
  match pat with Sugared.PAnnotated (_, ty) -> Some ty | _ -> None

(* The header of a top-level definition consists of its arguments and its
   result. All of them must be annotated, and together the annotations give the
   declared type of the definition. *)
let rec header_ty x ({ Sugared.it = term; at = loc } : Sugared.term) =
  match term with
  | Sugared.Annotated (_, ty) -> ty
  | Sugared.Lambda (pat, term') -> (
      match pattern_annotation pat with
      | Some ty1 ->
          let ty2 = header_ty x term' in
          { Sugared.it = Sugared.TyArrow (ty1, ty2); at = loc }
      | None ->
          Error.syntax ~loc:pat.at
            "An argument of the top-level definition %s needs a type annotation"
            x)
  | _ ->
      Error.syntax ~loc
        "The result of the top-level definition %s needs a type annotation" x

let rec free_ty_params { Sugared.it = ty; _ } =
  match ty with
  | Sugared.TyParam p -> [ p ]
  | Sugared.TyApply (_, tys) | Sugared.TyTuple tys ->
      List.concat_map free_ty_params tys
  | Sugared.TyArrow (ty1, ty2) -> free_ty_params ty1 @ free_ty_params ty2
  | Sugared.TyConst _ -> []

(* The type params of a top-level definition are the free params of its
   declared type. The returned state holds them, so that annotations in the
   body refer to the same params. *)
let desugar_ty_scheme state x term =
  let ty = header_ty x term in
  let params = List.sort_uniq String.compare (free_ty_params ty) in
  let state' = add_fresh_ty_params state params in
  let params' = List.map (lookup_ty_param ~loc:ty.at state') params in
  (state', (params', desugar_ty state' ty))

let desugar_ty_def ~loc (labels, state) = function
  (* We track labels separately since they will be used in the rest of the program *)
  | Sugared.TyInline ty -> (labels, Untyped.TyInline (desugar_ty state ty))
  | Sugared.TySum variants ->
      let aux (state, labels) (label, ty) =
        let label' = Untyped.Label.fresh label in
        let ty' = Option.map (desugar_ty state) ty in
        let state' = add_label ~loc state label label' in
        ((state', (label, label') :: labels), (label', ty'))
      in
      (* at the end, we ignore _state' since it is polluted with type params *)
      let (_state', labels'), variants' =
        List.fold_map aux (state, labels) variants
      in
      (labels', Untyped.TySum variants')

let desugar_command state { Sugared.it = cmd; at = loc } =
  match cmd with
  | Sugared.TyDef defs ->
      let def_name (_, ty_name, _) =
        let ty_name' = Untyped.TyName.fresh ty_name in
        (ty_name, ty_name')
      in
      (* TODO: the naming state/state'/… is confusing because it is
         not clear what parts need to be global (type names and
         labels) and which local (type parameters) *)
      let new_names = List.map def_name defs in
      let state' = add_fresh_ty_names ~loc state new_names in
      let aux (params, _, ty_def) (_, ty_name') (labels, state', defs) =
        let state'' = add_fresh_ty_params state' params in
        let labels', ty_def' = desugar_ty_def ~loc (labels, state'') ty_def in
        ( labels',
          state'',
          (List.map (lookup_ty_param ~loc state'') params, ty_name', ty_def')
          :: defs )
      in
      let labels, _, defs' =
        List.fold_right2 aux defs new_names ([], state', [])
      in
      let state'' =
        List.fold_left
          (fun s (lbl, lbl') -> add_label ~loc s lbl lbl')
          state' labels
      in
      (state'', Untyped.TyDef defs')
  | Sugared.TopLet (x, term) ->
      let x' = Untyped.Variable.fresh x in
      let state' = add_fresh_variables state (StringMap.singleton x x') in
      let state'', ty_sch = desugar_ty_scheme state' x term in
      let expr = desugar_pure_expression state'' term in
      (* we ignore state'' in the end since it is polluted with
      type params, which we needed only to desugar expr *)
      (state', Untyped.TopLet (x', ty_sch, expr))
  | Sugared.TopDo term ->
      let comp = desugar_computation state term in
      (state, Untyped.TopDo comp)
  | Sugared.TopLetRec (f, term) ->
      let state', ty_sch = desugar_ty_scheme state f term in
      let _state'', f', expr = desugar_let_rec_def state' (f, term) in
      (* we ignore _state'' since it is polluted with type params *)
      let state''' = add_fresh_variables state (StringMap.singleton f f') in
      (state''', Untyped.TopLet (f', ty_sch, expr))

let load_primitive state x prim =
  let str = Language.Primitives.primitive_name prim in
  add_fresh_variables state (StringMap.singleton str x)
