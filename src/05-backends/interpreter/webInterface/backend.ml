include Interpreter
open Vdom

let button txt msg =
  input [] ~a:[ onclick (fun _ -> msg); type_button; value txt ]

let view_computation_redex = function
  | Interpreter.Match -> "match"
  | Interpreter.ApplyFun -> "applyFun"
  | Interpreter.DoReturn -> "doReturn"

let rec view_computation_reduction = function
  | Interpreter.DoCtx red -> view_computation_reduction red
  | Interpreter.ComputationRedex redex -> view_computation_redex redex

let view_step step =
  match step.reduction with
  | Interpreter.ComputationReduction reduction ->
      text (view_computation_reduction reduction)
  | Interpreter.ReturnValue -> text "return"

let view_run_state (run_state : run_state) step =
  match run_state with
  | Running { computations = comp :: _; _ } ->
      let reduction =
        match step with
        | Some { reduction = ComputationReduction red; _ } -> Some red
        | Some { reduction = Interpreter.ReturnValue; _ } -> None
        | None -> None
      in

      let computation_tree =
        RedexSelectorTM.view_computation_with_redexes reduction comp
      in
      div ~a:[ class_ "box" ] [ elt "pre" computation_tree ]
  | Running { computations = []; _ } ->
      div ~a:[ class_ "box" ] [ elt "pre" [ text "done" ] ]
  | Returning (_, _) -> div ~a:[ class_ "box" ] [ elt "pre" [ text "return" ] ]
