open Utils
module Ast = Language.Ast
module Const = Language.Const

type environment = {
  variables : Ast.expression Ast.VariableMap.t;
  builtin_functions : (Ast.expression -> Ast.computation) Ast.VariableMap.t;
}

let initial_environment =
  {
    variables = Ast.VariableMap.empty;
    builtin_functions = Ast.VariableMap.empty;
  }

exception PatternMismatch

type computation_redex = Match | ApplyFun | DoReturn

type computation_reduction =
  | DoCtx of computation_reduction
  | ComputationRedex of computation_redex

let rec eval_tuple env = function
  | Ast.Tuple exprs -> exprs
  | Ast.Var x -> eval_tuple env (Ast.VariableMap.find x env.variables)
  | expr ->
      Error.runtime "Tuple expected but got %t" (Ast.print_expression expr)

let rec eval_variant env = function
  | Ast.Variant (lbl, expr) -> (lbl, expr)
  | Ast.Var x -> eval_variant env (Ast.VariableMap.find x env.variables)
  | expr ->
      Error.runtime "Variant expected but got %t" (Ast.print_expression expr)

let rec eval_const env = function
  | Ast.Const c -> c
  | Ast.Var x -> eval_const env (Ast.VariableMap.find x env.variables)
  | expr ->
      Error.runtime "Const expected but got %t" (Ast.print_expression expr)

let rec match_pattern_with_expression env pat expr =
  match pat with
  | Ast.PVar x -> Ast.VariableMap.singleton x expr
  | Ast.PAnnotated (pat, _) -> match_pattern_with_expression env pat expr
  | Ast.PAs (pat, x) ->
      let subst = match_pattern_with_expression env pat expr in
      Ast.VariableMap.add x expr subst
  | Ast.PTuple pats ->
      let exprs = eval_tuple env expr in
      List.fold_left2
        (fun subst pat expr ->
          let subst' = match_pattern_with_expression env pat expr in
          Ast.VariableMap.union (fun _ _ _ -> assert false) subst subst')
        Ast.VariableMap.empty pats exprs
  | Ast.PVariant (label, pat) -> (
      match (pat, eval_variant env expr) with
      | None, (label', None) when label = label' -> Ast.VariableMap.empty
      | Some pat, (label', Some expr) when label = label' ->
          match_pattern_with_expression env pat expr
      | _, _ -> raise PatternMismatch)
  | Ast.PConst c when Const.equal c (eval_const env expr) ->
      Ast.VariableMap.empty
  | Ast.PNonbinding -> Ast.VariableMap.empty
  | _ -> raise PatternMismatch

let rec remove_pattern_bound_variables subst = function
  | Ast.PVar x -> Ast.VariableMap.remove x subst
  | Ast.PAnnotated (pat, _) -> remove_pattern_bound_variables subst pat
  | Ast.PAs (pat, x) ->
      let subst = remove_pattern_bound_variables subst pat in
      Ast.VariableMap.remove x subst
  | Ast.PTuple pats -> List.fold_left remove_pattern_bound_variables subst pats
  | Ast.PVariant (_, None) -> subst
  | Ast.PVariant (_, Some pat) -> remove_pattern_bound_variables subst pat
  | Ast.PConst _ -> subst
  | Ast.PNonbinding -> subst

let rec refresh_pattern = function
  | Ast.PVar x ->
      let x' = Ast.Variable.refresh x in
      (Ast.PVar x', [ (x, x') ])
  | Ast.PAnnotated (pat, _) -> refresh_pattern pat
  | Ast.PAs (pat, x) ->
      let pat', vars = refresh_pattern pat in
      let x' = Ast.Variable.refresh x in
      (Ast.PAs (pat', x'), (x, x') :: vars)
  | Ast.PTuple pats ->
      let fold pat (pats', vars) =
        let pat', vars' = refresh_pattern pat in
        (pat' :: pats', vars' @ vars)
      in
      let pats', vars = List.fold_right fold pats ([], []) in
      (Ast.PTuple pats', vars)
  | Ast.PVariant (lbl, Some pat) ->
      let pat', vars = refresh_pattern pat in
      (PVariant (lbl, Some pat'), vars)
  | (PVariant (_, None) | PConst _ | PNonbinding) as pat -> (pat, [])

let rec refresh_expression vars = function
  | Ast.Var x as expr -> (
      match List.assoc_opt x vars with None -> expr | Some x' -> Var x')
  | Ast.Const _ as expr -> expr
  | Ast.Annotated (expr, ty) -> Ast.Annotated (refresh_expression vars expr, ty)
  | Ast.Tuple exprs -> Ast.Tuple (List.map (refresh_expression vars) exprs)
  | Ast.Variant (label, expr) ->
      Ast.Variant (label, Option.map (refresh_expression vars) expr)
  | Ast.Lambda abs -> Ast.Lambda (refresh_abstraction vars abs)
  | Ast.RecLambda (x, abs) ->
      let x' = Ast.Variable.refresh x in
      Ast.RecLambda (x', refresh_abstraction ((x, x') :: vars) abs)

and refresh_computation vars = function
  | Ast.Return expr -> Ast.Return (refresh_expression vars expr)
  | Ast.Do (comp, abs) ->
      Ast.Do (refresh_computation vars comp, refresh_abstraction vars abs)
  | Ast.Match (expr, cases) ->
      Ast.Match
        (refresh_expression vars expr, List.map (refresh_abstraction vars) cases)
  | Ast.Apply (expr1, expr2) ->
      Ast.Apply (refresh_expression vars expr1, refresh_expression vars expr2)

and refresh_abstraction vars (pat, comp) =
  let pat', vars' = refresh_pattern pat in
  (pat', refresh_computation (vars @ vars') comp)

let rec substitute_expression subst = function
  | Ast.Var x as expr -> (
      match Ast.VariableMap.find_opt x subst with
      | None -> expr
      | Some expr -> expr)
  | Ast.Const _ as expr -> expr
  | Ast.Annotated (expr, ty) -> Annotated (substitute_expression subst expr, ty)
  | Ast.Tuple exprs -> Tuple (List.map (substitute_expression subst) exprs)
  | Ast.Variant (label, expr) ->
      Variant (label, Option.map (substitute_expression subst) expr)
  | Ast.Lambda abs -> Lambda (substitute_abstraction subst abs)
  | Ast.RecLambda (x, abs) -> RecLambda (x, substitute_abstraction subst abs)

and substitute_computation subst = function
  | Ast.Return expr -> Ast.Return (substitute_expression subst expr)
  | Ast.Do (comp, abs) ->
      Ast.Do
        (substitute_computation subst comp, substitute_abstraction subst abs)
  | Ast.Match (expr, cases) ->
      Ast.Match
        ( substitute_expression subst expr,
          List.map (substitute_abstraction subst) cases )
  | Ast.Apply (expr1, expr2) ->
      Ast.Apply
        (substitute_expression subst expr1, substitute_expression subst expr2)

and substitute_abstraction subst (pat, comp) =
  let subst' = remove_pattern_bound_variables subst pat in
  (pat, substitute_computation subst' comp)

let substitute subst comp =
  let subst = Ast.VariableMap.map (refresh_expression []) subst in
  substitute_computation subst comp

let rec eval_function env = function
  | Ast.Lambda (pat, comp) ->
      fun arg ->
        let subst = match_pattern_with_expression env pat arg in
        substitute subst comp
  | Ast.RecLambda (f, (pat, comp)) as expr ->
      fun arg ->
        let subst =
          match_pattern_with_expression env pat arg
          |> Ast.VariableMap.add f expr
        in
        substitute subst comp
  | Ast.Var x -> (
      match Ast.VariableMap.find_opt x env.variables with
      | Some expr -> eval_function env expr
      | None -> Ast.VariableMap.find x env.builtin_functions)
  | expr ->
      Error.runtime "Function expected but got %t" (Ast.print_expression expr)

let step_in_context step env redCtx ctx term =
  let terms' = step env term in
  List.map (fun (red, term') -> (redCtx red, fun () -> ctx (term' ()))) terms'

let rec step_computation env = function
  | Ast.Return _ -> []
  | Ast.Match (expr, cases) ->
      let rec find_case = function
        | (pat, comp) :: cases -> (
            match match_pattern_with_expression env pat expr with
            | subst ->
                [ (ComputationRedex Match, fun () -> substitute subst comp) ]
            | exception PatternMismatch -> find_case cases)
        | [] -> []
      in
      find_case cases
  | Ast.Apply (expr1, expr2) ->
      let f = eval_function env expr1 in
      [ (ComputationRedex ApplyFun, fun () -> f expr2) ]
  | Ast.Do (comp1, comp2) -> (
      let comps1' =
        step_in_context step_computation env
          (fun red -> DoCtx red)
          (fun comp1' -> Ast.Do (comp1', comp2))
          comp1
      in
      match comp1 with
      | Ast.Return expr ->
          let pat, comp2' = comp2 in
          let subst = match_pattern_with_expression env pat expr in
          (ComputationRedex DoReturn, fun () -> substitute subst comp2')
          :: comps1'
      | _ -> comps1')

type load_state = {
  environment : environment;
  computations : Ast.computation list;
}

let initial_load_state =
  { environment = initial_environment; computations = [] }

let load_primitive load_state x prim =
  {
    load_state with
    environment =
      {
        load_state.environment with
        builtin_functions =
          Ast.VariableMap.add x
            (Primitives.primitive_function prim)
            load_state.environment.builtin_functions;
      };
  }

let load_ty_def load_state _ = load_state

let load_top_let load_state x expr =
  {
    load_state with
    environment =
      {
        load_state.environment with
        variables = Ast.VariableMap.add x expr load_state.environment.variables;
      };
  }

let load_top_do load_state comp =
  { load_state with computations = load_state.computations @ [ comp ] }

type run_state = load_state
type step_label = ComputationReduction of computation_reduction | Return
type step = { label : step_label; next_state : unit -> run_state }

let run load_state = load_state

let steps = function
  | { computations = []; _ } -> []
  | { computations = Ast.Return _ :: comps; environment } ->
      [
        {
          label = Return;
          next_state = (fun () -> { computations = comps; environment });
        };
      ]
  | { computations = comp :: comps; environment } ->
      List.map
        (fun (red, comp') ->
          {
            label = ComputationReduction red;
            next_state =
              (fun () -> { computations = comp' () :: comps; environment });
          })
        (step_computation environment comp)
