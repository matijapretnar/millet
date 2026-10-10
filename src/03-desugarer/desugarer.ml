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
  local_vars : Untyped.variable StringMap.t;
  top_vars : Untyped.top_var StringMap.t;
  labels : Untyped.label StringMap.t;
}

type computation_wrapper =
  Untyped.computation Bindlib.box -> Untyped.computation Bindlib.box

type boxed_expression = {
  (* An expression and the computation_wrapper needed to compute the expression. *)
  wrap : computation_wrapper;
  expr : Untyped.expression Bindlib.box;
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
    local_vars = StringMap.empty;
    top_vars = StringMap.empty;
    labels =
      StringMap.empty
      |> StringMap.add Sugared.nil_label Untyped.nil_label
      |> StringMap.add Sugared.cons_label Untyped.cons_label;
  }

let no_wrap comp = comp
let boxed_expression ?(wrap = no_wrap) expr = { wrap; expr }

module Boxed = struct
  let annotated t ty = Bindlib.box_apply (fun t -> Untyped.Annotated (t, ty)) t

  let tuple ts =
    Bindlib.box_apply (fun ts -> Untyped.Tuple ts) (Bindlib.box_list ts)

  let variant l e_opt =
    match e_opt with
    | None -> Bindlib.box (Untyped.Variant (l, None))
    | Some e -> Bindlib.box_apply (fun e -> Untyped.Variant (l, Some e)) e

  let lambda a = Bindlib.box_apply (fun a -> Untyped.Lambda a) a
  let rec_lambda b = Bindlib.box_apply (fun b -> Untyped.RecLambda b) b
  let return e = Bindlib.box_apply (fun e -> Untyped.Return e) e
  let do_ c1 a = Bindlib.box_apply2 (fun c1 a -> Untyped.Do (c1, a)) c1 a

  let match_ e cases =
    Bindlib.box_apply2
      (fun e cases -> Untyped.Match (e, cases))
      e (Bindlib.box_list cases)

  let apply e1 e2 =
    Bindlib.box_apply2 (fun e1 e2 -> Untyped.Apply (e1, e2)) e1 e2
end

let find_symbol ~loc map name =
  match StringMap.find_opt name map with
  | None -> Error.syntax ~loc "Unknown name --%s--" name
  | Some symbol -> symbol

let lookup_ty_name ~loc state = find_symbol ~loc state.ty_names
let lookup_ty_param ~loc state = find_symbol ~loc state.ty_params
let lookup_local_var ~loc state = find_symbol ~loc state.local_vars
let lookup_top_var ~loc state = find_symbol ~loc state.top_vars
let lookup_label ~loc state = find_symbol ~loc state.labels

let lookup_term ~loc state name =
  if StringMap.mem name state.local_vars then
    Bindlib.box_var (lookup_local_var ~loc state name)
  else Bindlib.box (Untyped.TopVar (lookup_top_var ~loc state name))

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
  match pat with
  | Sugared.PVar x ->
      let x_var = Bindlib.new_var (fun x -> Untyped.Var x) x in
      (add_unique ~loc "Variable" x x_var vars, [ x_var ], Untyped.PVar)
  | Sugared.PAnnotated (pat, ty) ->
      let vars, binders, pat' = desugar_pattern state vars pat
      and ty' = desugar_ty state ty in
      (vars, binders, Untyped.PAnnotated (pat', ty'))
  | Sugared.PAs (pat, x) ->
      let vars, binders, pat' = desugar_pattern state vars pat in
      let x_var = Bindlib.new_var (fun x -> Untyped.Var x) x in
      ( add_unique ~loc "Variable" x x_var vars,
        binders @ [ x_var ],
        Untyped.PAs pat' )
  | Sugared.PTuple ps ->
      let aux p (vars, binders, ps') =
        let vars, binders', p' = desugar_pattern state vars p in
        (vars, binders' @ binders, p' :: ps')
      in
      let vars, binders, ps' = List.fold_right aux ps (vars, [], []) in
      (vars, binders, Untyped.PTuple ps')
  | Sugared.PVariant (lbl, None) ->
      let lbl' = lookup_label ~loc state lbl in
      (vars, [], Untyped.PVariant (lbl', None))
  | Sugared.PVariant (lbl, Some pat) ->
      let lbl' = lookup_label ~loc state lbl in
      let vars, binders, pat' = desugar_pattern state vars pat in
      (vars, binders, Untyped.PVariant (lbl', Some pat'))
  | Sugared.PConst c -> (vars, [], Untyped.PConst c)
  | Sugared.PNonbinding -> (vars, [], Untyped.PNonbinding)

let add_local_vars state vars =
  {
    state with
    local_vars = StringMap.union (fun _ v _ -> Some v) vars state.local_vars;
  }

let close_abstraction vars pat comp =
  (* It creates a complete boxed abstraction *)
  Bindlib.box_apply (fun binder -> (pat, binder)) (Bindlib.bind_mvar vars comp)

let rec desugar_expression state { Sugared.it = term; at = loc } =
  match term with
  | Sugared.Var x -> boxed_expression (lookup_term ~loc state x)
  | Sugared.Const k -> boxed_expression (Bindlib.box (Untyped.Const k))
  | Sugared.Annotated (term, ty) ->
      let expr = desugar_expression state term in
      let ty' = desugar_ty state ty in
      { expr with expr = Boxed.annotated expr.expr ty' }
  | Sugared.Tuple ts ->
      let expressions = List.map (desugar_expression state) ts in
      let wrap comp =
        List.fold_right (fun { wrap; _ } comp -> wrap comp) expressions comp
      in
      let exprs = List.map (fun { expr; _ } -> expr) expressions in
      boxed_expression ~wrap (Boxed.tuple exprs)
  | Sugared.Variant (lbl, None) ->
      let lbl' = lookup_label ~loc state lbl in
      boxed_expression (Boxed.variant lbl' None)
  | Sugared.Variant (lbl, Some t) ->
      let expr = desugar_expression state t in
      let lbl' = lookup_label ~loc state lbl in
      { expr with expr = Boxed.variant lbl' (Some expr.expr) }
  | Sugared.Lambda a ->
      let a = desugar_abstraction state a in
      boxed_expression (Boxed.lambda a)
  | Sugared.Function cases ->
      boxed_expression (Boxed.lambda (desugar_function_abstraction state cases))
  | ( Sugared.Apply _ | Sugared.Match _ | Sugared.Let _ | Sugared.LetRec _
    | Sugared.Conditional _ ) as term ->
      let comp = desugar_computation state { Sugared.it = term; at = loc } in
      let b_var = Bindlib.new_var (fun x -> Untyped.Var x) "b" in
      let b_box = Bindlib.box_var b_var in
      let wrap c_cont =
        let abstraction = close_abstraction [| b_var |] Untyped.PVar c_cont in
        Boxed.do_ comp abstraction
      in
      boxed_expression ~wrap b_box

and desugar_computation state { Sugared.it = term; at = loc } =
  match term with
  | Sugared.Apply
      ({ it = Sugared.Var "(&&)"; _ }, { it = Sugared.Tuple [ t1; t2 ]; _ }) ->
      let left = desugar_expression state t1 in
      let c1 = desugar_computation state t2 in
      let c2 =
        Boxed.return (Bindlib.box (Untyped.Const (Const.Boolean false)))
      in
      left.wrap (desugar_if_then_else left.expr c1 c2)
  | Sugared.Apply
      ({ it = Sugared.Var "(||)"; _ }, { it = Sugared.Tuple [ t1; t2 ]; _ }) ->
      let left = desugar_expression state t1 in
      let c1 =
        Boxed.return (Bindlib.box (Untyped.Const (Const.Boolean true)))
      in
      let c2 = desugar_computation state t2 in
      left.wrap (desugar_if_then_else left.expr c1 c2)
  | Sugared.Apply (t1, t2) ->
      let fn = desugar_expression state t1 in
      let arg = desugar_expression state t2 in
      fn.wrap (arg.wrap (Boxed.apply fn.expr arg.expr))
  | Sugared.Match (t, cs) ->
      let expr = desugar_expression state t in
      let cs' = List.map (desugar_abstraction state) cs in
      expr.wrap (Boxed.match_ expr.expr cs')
  | Sugared.Conditional (t, t1, t2) ->
      let expr = desugar_expression state t in
      let c1 = desugar_computation state t1 in
      let c2 = desugar_computation state t2 in
      expr.wrap (desugar_if_then_else expr.expr c1 c2)
  | Sugared.Let (pat, t1, t2) ->
      let c1 = desugar_computation state t1 in
      let a = desugar_abstraction state (pat, t2) in
      Boxed.do_ c1 a
  | Sugared.LetRec (f, t1, t2) ->
      let f_var = Bindlib.new_var (fun x -> Untyped.Var x) f in
      let state_rec =
        { state with local_vars = StringMap.add f f_var state.local_vars }
      in
      let abstraction, annotations = desugar_recursive_function state_rec t1 in
      let rec_binder = Bindlib.bind_var f_var abstraction in
      let rec_expr =
        List.fold_left Boxed.annotated (Boxed.rec_lambda rec_binder) annotations
      in
      let a_let =
        desugar_abstraction state ({ Sugared.it = Sugared.PVar f; at = loc }, t2)
      in
      Boxed.do_ (Boxed.return rec_expr) a_let
  | term ->
      let expr = desugar_expression state { Sugared.it = term; at = loc } in
      expr.wrap (Boxed.return expr.expr)

and desugar_abstraction state (pat, term) =
  let vars, binders, pat' = desugar_pattern state StringMap.empty pat in
  let state' = add_local_vars state vars in
  let comp = desugar_computation state' term in
  close_abstraction (Array.of_list binders) pat' comp

and desugar_function_abstraction state cases =
  let x_name = "arg" in
  let x_var = Bindlib.new_var (fun x -> Untyped.Var x) x_name in
  let x_box = Bindlib.box_var x_var in
  let cases' = List.map (desugar_abstraction state) cases in
  let match_expr = Boxed.match_ x_box cases' in
  close_abstraction [| x_var |] Untyped.PVar match_expr

and desugar_recursive_function state { Sugared.it = term; at = loc } =
  match term with
  | Sugared.Annotated (term, ty) ->
      let abstraction, annotations = desugar_recursive_function state term in
      let ty' = desugar_ty state ty in
      (abstraction, annotations @ [ ty' ])
  | Sugared.Lambda abstraction -> (desugar_abstraction state abstraction, [])
  | Sugared.Function cases -> (desugar_function_abstraction state cases, [])
  | _ -> Error.syntax ~loc "let rec expects a function expression"

and desugar_if_then_else e c1 c2 =
  let true_p = Untyped.PConst Const.of_true in
  let false_p = Untyped.PConst Const.of_false in
  let a1 = close_abstraction [||] true_p c1 in
  let a2 = close_abstraction [||] false_p c2 in
  Boxed.match_ e [ a1; a2 ]

let add_label ~loc state label label' =
  let labels' = add_unique ~loc "Label" label label' state.labels in
  { state with labels = labels' }

let add_fresh_ty_names ~loc state vars =
  let aux ty_names (x, x') = add_unique ~loc "Type" x x' ty_names in
  let ty_names' = List.fold_left aux state.ty_names vars in
  { state with ty_names = ty_names' }

let add_fresh_ty_params state vars =
  let aux ty_params (x, x') = StringMap.add x x' ty_params in
  let ty_params' = List.fold_left aux state.ty_params vars in
  { state with ty_params = ty_params' }

let desugar_ty_def ~loc state = function
  | Sugared.TyInline ty -> (state, Untyped.TyInline (desugar_ty state ty))
  | Sugared.TySum variants ->
      let aux state (label, ty) =
        let label' = Untyped.Label.fresh label in
        let ty' = Option.map (desugar_ty state) ty in
        let state' = add_label ~loc state label label' in
        (state', (label', ty'))
      in
      let state', variants' = List.fold_map aux state variants in
      (state', Untyped.TySum variants')

let desugar_command state { Sugared.it = cmd; at = loc } =
  match cmd with
  | Sugared.TyDef defs ->
      let def_name (_, ty_name, _) =
        let ty_name' = Untyped.TyName.fresh ty_name in
        (ty_name, ty_name')
      in
      let new_names = List.map def_name defs in
      let state' = add_fresh_ty_names ~loc state new_names in
      let aux (params, _, ty_def) (_, ty_name') (state', defs) =
        let params' = List.map (fun a -> (a, Untyped.TyParam.fresh a)) params in
        let state'' = add_fresh_ty_params state' params' in
        let state''', ty_def' = desugar_ty_def ~loc state'' ty_def in
        (state''', (List.map snd params', ty_name', ty_def') :: defs)
      in
      let state'', defs' = List.fold_right2 aux defs new_names (state', []) in
      (state'', Untyped.TyDef defs')
  | Sugared.TopLet (x, term) ->
      let x' = Untyped.TopVar.fresh x in
      let state' =
        { state with top_vars = StringMap.add x x' state.top_vars }
      in
      let expr_box = (desugar_expression state' term).expr in
      (* Desugaring moves every computation out of an expression and binds its
         result to a fresh variable, which stays free in the expression. No
         local variables are in scope at top level, so the expression is closed
         exactly when it is pure. *)
      if not (Bindlib.is_closed expr_box) then
        Error.syntax ~loc:term.at "Only pure expressions are allowed";
      (state', Untyped.TopLet (x', Bindlib.unbox expr_box))
  | Sugared.TopDo term ->
      let comp_box = desugar_computation state term in
      (state, Untyped.TopDo (Bindlib.unbox comp_box))
  | Sugared.TopLetRec (f, term) ->
      let f_var = Bindlib.new_var (fun x -> Untyped.Var x) f in
      let state_rec =
        { state with local_vars = StringMap.add f f_var state.local_vars }
      in
      let abstraction, annotations =
        desugar_recursive_function state_rec term
      in
      let rec_binder = Bindlib.bind_var f_var abstraction in
      let rec_expr =
        List.fold_left Boxed.annotated (Boxed.rec_lambda rec_binder) annotations
      in
      let f' = Untyped.TopVar.fresh f in
      let state' =
        { state with top_vars = StringMap.add f f' state.top_vars }
      in
      (state', Untyped.TopLet (f', Bindlib.unbox rec_expr))

let load_primitive state x prim =
  let str = Language.Primitives.primitive_name prim in
  { state with top_vars = StringMap.add str x state.top_vars }
