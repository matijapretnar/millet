(** Desugaring of syntax into the core language. *)

open Utils
open Utils.LongName
module Sugared = Parser.SugaredAst
module Untyped = Language.Ast
module Const = Language.Const
module NameMap = Map.Make (LongName)

let add_unique ~loc kind p symb string_map =
  NameMap.update p
    (function
      | None -> Some symb
      | Some _ ->
          Error.syntax ~loc "%s %s defined multiple times." kind
            (LongName.to_string p))
    string_map

type state = {
  prefix : string list;
  ty_names : Untyped.ty_name NameMap.t;
  ty_params : Untyped.ty_param NameMap.t;
  variables : Untyped.variable NameMap.t;
  labels : Untyped.label NameMap.t;
}

let initial_state =
  {
    prefix = [];
    ty_names =
      LongName.(
        NameMap.empty
        |> NameMap.add (from_id Sugared.bool_ty_name) Untyped.bool_ty_name
        |> NameMap.add (from_id Sugared.int_ty_name) Untyped.int_ty_name
        |> NameMap.add (from_id Sugared.unit_ty_name) Untyped.unit_ty_name
        |> NameMap.add (from_id Sugared.string_ty_name) Untyped.string_ty_name
        |> NameMap.add (from_id Sugared.float_ty_name) Untyped.float_ty_name
        |> NameMap.add (from_id Sugared.empty_ty_name) Untyped.empty_ty_name
        |> NameMap.add (from_id Sugared.list_ty_name) Untyped.list_ty_name);
    ty_params = NameMap.empty;
    variables = NameMap.empty;
    labels =
      LongName.(
        NameMap.empty
        |> NameMap.add (from_id Sugared.nil_label) Untyped.nil_label
        |> NameMap.add (from_id Sugared.cons_label) Untyped.cons_label);
  }

let find_symbol ~loc state get_map path =
  let mp = get_map state in
  (* Try to find the symbol in the local scope. *)
  match NameMap.find_opt (LongName.with_prefix path state.prefix) mp with
  | None -> (
      (* Try to find the symbol in the global scope. *)
      match NameMap.find_opt path mp with
      | None ->
          Error.syntax ~loc "Unknown name --%s--" (LongName.to_string path)
      | Some symbol -> symbol)
  | Some symbol -> symbol

let lookup_ty_name ~loc state = find_symbol ~loc state (fun s -> s.ty_names)
let lookup_ty_param ~loc state = find_symbol ~loc state (fun s -> s.ty_params)
let lookup_variable ~loc state = find_symbol ~loc state (fun s -> s.variables)
let lookup_label ~loc state = find_symbol ~loc state (fun s -> s.labels)

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
      let x' = Untyped.Variable.fresh (LongName.id x) in
      (NameMap.singleton x x', Untyped.PVar x')
  | Sugared.PAnnotated (pat, ty) ->
      let vars, pat' = desugar_pattern state vars pat
      and ty' = desugar_ty state ty in
      (vars, Untyped.PAnnotated (pat', ty'))
  | Sugared.PAs (pat, x) ->
      let vars, pat' = desugar_pattern state vars pat in
      let x' = Untyped.Variable.fresh (LongName.id x) in
      (add_unique ~loc "Variable" x x' vars, Untyped.PAs (pat', x'))
  | Sugared.PTuple ps ->
      let aux p (vars, ps') =
        let vars', p' = desugar_pattern state vars p in
        (NameMap.fold (add_unique ~loc "Variable") vars' vars, p' :: ps')
      in
      let vars, ps' = List.fold_right aux ps (NameMap.empty, []) in
      (vars, Untyped.PTuple ps')
  | Sugared.PVariant (lbl, None) ->
      let lbl' = lookup_label ~loc state lbl in
      (NameMap.empty, Untyped.PVariant (lbl', None))
  | Sugared.PVariant (lbl, Some pat) ->
      let lbl' = lookup_label ~loc state lbl in
      let vars, pat' = desugar_pattern state vars pat in
      (vars, Untyped.PVariant (lbl', Some pat'))
  | Sugared.PConst c -> (NameMap.empty, Untyped.PConst c)
  | Sugared.PNonbinding -> (NameMap.empty, Untyped.PNonbinding)

let add_fresh_variables state vars =
  let aux x x' variables =
    NameMap.add (LongName.with_prefix x state.prefix) x' variables
  in
  let variables' = NameMap.fold aux vars state.variables in
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
      let ty' = desugar_ty state ty in
      (binds, Untyped.Annotated (expr, ty'))
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
      ( { it = Sugared.Var ([], "(&&)"); _ },
        { it = Sugared.Tuple [ t1; t2 ]; _ } ) ->
      let binds1, e1 = desugar_expression state t1 in
      let c1 = desugar_computation state t2 in
      let c2 = Untyped.Return (Untyped.Const (Const.Boolean false)) in
      (binds1, if_then_else e1 c1 c2)
  | Sugared.Apply
      ( { it = Sugared.Var ([], "(||)"); _ },
        { it = Sugared.Tuple [ t1; t2 ]; _ } ) ->
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
  let vars, pat' = desugar_pattern state NameMap.empty pat in
  let state' = add_fresh_variables state vars in
  let comp = desugar_computation state' term in
  (pat', comp)

and desugar_guarded_abstraction state (pat, term1, term2) =
  let vars, pat' = desugar_pattern state NameMap.empty pat in
  let state' = add_fresh_variables state vars in
  let comp1 = desugar_computation state' term1
  and comp2 = desugar_computation state' term2 in
  (pat', comp1, comp2)

and desugar_promise_abstraction ~loc state abs2 =
  match desugar_abstraction state abs2 with
  | Untyped.PVar p, comp' -> (p, comp')
  | Untyped.PNonbinding, comp' ->
      let p = Untyped.Variable.fresh "_" in
      (p, comp')
  | _ -> Error.syntax ~loc "Variable or underscore expected"

and desugar_let_rec_def state (f, { it = exp; at = loc }) =
  let f' = Untyped.Variable.fresh (LongName.id f) in
  let state' = add_fresh_variables state (NameMap.singleton f f') in
  let abs' =
    match exp with
    | Sugared.Lambda a -> desugar_abstraction state' a
    | Sugared.Function cs ->
        let x = Untyped.Variable.fresh "rf" in
        let cs = List.map (desugar_abstraction state') cs in
        let new_match = Untyped.Match (Untyped.Var x, cs) in
        (Untyped.PVar x, new_match)
    | _ ->
        Error.syntax ~loc
          "This kind of expression is not allowed in a recursive definition"
  in
  let expr = Untyped.RecLambda (f', abs') in
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
  let labels' =
    add_unique ~loc "Label"
      (LongName.with_prefix label state.prefix)
      label' state.labels
  in
  { state with labels = labels' }

let add_fresh_ty_names ~loc state vars =
  let aux ty_names (x, x') =
    add_unique ~loc "Type" (LongName.with_prefix x state.prefix) x' ty_names
  in
  let ty_names' = List.fold_left aux state.ty_names vars in
  { state with ty_names = ty_names' }

let add_fresh_ty_params state vars =
  let aux ty_params (x, x') =
    NameMap.add (LongName.with_prefix x state.prefix) x' ty_params
  in
  let ty_params' = List.fold_left aux state.ty_params vars in
  { state with ty_params = ty_params' }

let desugar_ty_def ~loc state = function
  | Sugared.TyInline ty -> (state, Untyped.TyInline (desugar_ty state ty))
  | Sugared.TySum variants ->
      let aux state (label, ty) =
        let label' = Untyped.Label.fresh (LongName.id label) in
        let ty' = Option.map (desugar_ty state) ty in
        let state' = add_label ~loc state label label' in
        (state', (label', ty'))
      in
      let state', variants' = List.fold_map aux state variants in
      (state', Untyped.TySum variants')

let desugar_command_ty_def ~loc (state : state)
    (defs : (LongName.t list * LongName.t * Sugared.ty_def) list) =
  let def_name (_, ty_name, _) =
    let ty_name' = Untyped.TyName.fresh (LongName.id ty_name) in
    (ty_name, ty_name')
  in
  let new_names = List.map def_name defs in
  let state' = add_fresh_ty_names ~loc state new_names in
  let aux (params, _, ty_def) (_, ty_name') (state', defs) =
    let params' =
      List.map (fun a -> (a, Untyped.TyParam.fresh (LongName.id a))) params
    in
    let state'' = add_fresh_ty_params state' params' in
    let state''', ty_def' = desugar_ty_def ~loc state'' ty_def in
    (state''', (List.map snd params', ty_name', ty_def') :: defs)
  in
  let state'', defs' = List.fold_right2 aux defs new_names (state', []) in
  (state'', defs')

let desugar_command_top_let state (x, term) =
  let x' = Untyped.Variable.fresh (LongName.id x) in
  let state' = add_fresh_variables state (NameMap.singleton x x') in
  let expr = desugar_pure_expression state' term in
  (state', (x', expr))

let desugar_command_top_let_rec state (f, term) =
  let state', f, expr = desugar_let_rec_def state (f, term) in
  (state', (f, expr))

let rec desugar_mod_def loc state def =
  match def with
  | Sugared.MTyDef defs ->
      let state', defs' = desugar_command_ty_def ~loc state defs in
      (state', Untyped.MTyDef defs')
  | Sugared.Module (mod_name, defs) ->
      let mod_name' = Untyped.ModName.fresh (LongName.id mod_name) in
      let state', des_defs =
        List.fold_left_map
          (fun st def -> desugar_mod_def loc st def)
          { state with prefix = state.prefix @ [ LongName.id mod_name ] }
          defs
      in
      let state'' = { state' with prefix = LongName.pop state'.prefix } in
      (state'', Untyped.MModule (mod_name', des_defs))
  | Sugared.MTopLet (x, term) ->
      let state', (x, expr) = desugar_command_top_let state (x, term) in
      (state', Untyped.MTopLet (x, expr))
  | Sugared.MTopLetRec (f, term) ->
      let state', (f', expr) = desugar_command_top_let_rec state (f, term) in
      (state', Untyped.MTopLet (f', expr))

let desugar_command state { Sugared.it = cmd; at = loc } =
  match cmd with
  | Sugared.TyDef defs ->
      let state', defs' = desugar_command_ty_def ~loc state defs in
      (state', Untyped.TyDef defs')
  (* TODO: Module name should be just a string and not LongName. *)
  | Sugared.Module (mod_name, defs) ->
      let mod_name' = Untyped.ModName.fresh (LongName.id mod_name) in
      let state', des_defs =
        List.fold_left_map
          (fun st def -> desugar_mod_def loc st def)
          { state with prefix = state.prefix @ [ LongName.id mod_name ] }
          defs
      in
      let state'' = { state' with prefix = LongName.pop state'.prefix } in
      (state'', Untyped.Module (mod_name', des_defs))
  | Sugared.TopLet (x, term) ->
      let state', (x, expr) = desugar_command_top_let state (x, term) in
      (state', Untyped.TopLet (x, expr))
  | Sugared.TopDo term ->
      let comp = desugar_computation state term in
      (state, Untyped.TopDo comp)
  | Sugared.TopLetRec (f, term) ->
      let state', (f', expr) = desugar_command_top_let_rec state (f, term) in
      (state', Untyped.TopLet (f', expr))

let load_primitive state x prim =
  let str = Language.Primitives.primitive_name prim in
  add_fresh_variables state (NameMap.singleton (LongName.from_id str) x)
