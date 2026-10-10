open Utils
module Ast = Language.Ast
module Const = Language.Const

type top_value =
  | Defined of Ast.expression
  | Primitive of (Ast.expression -> Ast.computation)

type environment = { top_vars : top_value Ast.TopVarMap.t }

let initial_environment = { top_vars = Ast.TopVarMap.empty }

exception PatternMismatch

type computation_redex = Match | ApplyFun | DoReturn

type computation_reduction =
  | DoCtx of computation_reduction
  | ComputationRedex of computation_redex

(* Skip annotations and replace top-level variables by their definitions, until
   the outermost constructor is not one of them. Primitives stay top-level
   variables. *)
let rec head env expr =
  match expr.Ast.it with
  | Ast.Annotated (expr, _) -> head env expr
  | Ast.TopVar x -> (
      match Ast.TopVarMap.find x env.top_vars with
      | Defined expr -> head env expr
      | Primitive _ -> expr)
  | _ -> expr

let eval_tuple env expr =
  match (head env expr).it with
  | Ast.Tuple exprs -> exprs
  | _ -> Error.runtime "Tuple expected but got %t" (Ast.print_expression expr)

let eval_variant env expr =
  match (head env expr).it with
  | Ast.Variant (lbl, expr) -> (lbl, expr)
  | _ -> Error.runtime "Variant expected but got %t" (Ast.print_expression expr)

let eval_const env expr =
  match (head env expr).it with
  | Ast.Const c -> c
  | _ -> Error.runtime "Const expected but got %t" (Ast.print_expression expr)

let rec match_pattern_with_expression env pat expr =
  match pat.Ast.it with
  | Ast.PVar -> [ expr ]
  | Ast.PAnnotated (pat, _) -> match_pattern_with_expression env pat expr
  | Ast.PAs pat -> match_pattern_with_expression env pat expr @ [ expr ]
  | Ast.PTuple pats ->
      let exprs = eval_tuple env expr in
      List.map2 (match_pattern_with_expression env) pats exprs |> List.concat
  | Ast.PVariant (label, pat) -> (
      match (pat, eval_variant env expr) with
      | None, (label', None) when label = label' -> []
      | Some pat, (label', Some expr) when label = label' ->
          match_pattern_with_expression env pat expr
      | _, _ -> raise PatternMismatch)
  | Ast.PConst c when Const.equal c (eval_const env expr) -> []
  | Ast.PNonbinding -> []
  | _ -> raise PatternMismatch

let perform_abstraction env abstraction expr =
  let pat, binder = abstraction in
  let values = match_pattern_with_expression env pat expr |> Array.of_list in
  Bindlib.msubst binder values

let eval_function env expr =
  let expr' = head env expr in
  match expr'.it with
  | Ast.Lambda abstraction -> fun arg -> perform_abstraction env abstraction arg
  | Ast.RecLambda binder ->
      fun arg ->
        let abstraction = Bindlib.subst binder expr' in
        perform_abstraction env abstraction arg
  | Ast.TopVar x -> (
      match Ast.TopVarMap.find x env.top_vars with
      | Primitive f -> f
      | Defined _ -> assert false (* head resolves defined variables *))
  | _ ->
      Error.runtime "Function expected but got %t" (Ast.print_expression expr)

let step_in_context step env redCtx ctx term =
  let terms' = step env term in
  List.map (fun (red, term') -> (redCtx red, fun () -> ctx (term' ()))) terms'

let rec step_computation env comp =
  match comp.Ast.it with
  | Ast.Return _ -> []
  | Ast.Match (expr, cases) ->
      let rec find_case = function
        | abstraction :: cases -> (
            match perform_abstraction env abstraction expr with
            | comp -> [ (ComputationRedex Match, fun () -> comp) ]
            | exception PatternMismatch -> find_case cases)
        | [] -> []
      in
      find_case cases
  | Ast.Apply (expr1, expr2) ->
      let f = eval_function env expr1 in
      [ (ComputationRedex ApplyFun, fun () -> f expr2) ]
  | Ast.Do (comp1, abstraction) -> (
      let comps1' =
        step_in_context step_computation env
          (fun red -> DoCtx red)
          (fun comp1' -> { comp with it = Ast.Do (comp1', abstraction) })
          comp1
      in
      match comp1.it with
      | Ast.Return expr ->
          ( ComputationRedex DoReturn,
            fun () -> perform_abstraction env abstraction expr )
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
        top_vars =
          Ast.TopVarMap.add x
            (Primitive (Primitives.primitive_function prim))
            load_state.environment.top_vars;
      };
  }

let load_ty_def load_state _ = load_state

let load_top_let load_state x expr =
  {
    load_state with
    environment =
      {
        top_vars =
          Ast.TopVarMap.add x (Defined expr) load_state.environment.top_vars;
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
  | { computations = { it = Ast.Return _; _ } :: comps; environment } ->
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
