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

let view_step step = text (view_computation_reduction step.reduction)

let view_run_state (comp : run_state) step =
  let computation_tree =
    RedexSelectorTM.view_computation_with_redexes
      (Option.map (fun step -> step.reduction) step)
      comp.computation
  in
  div ~a:[ class_ "box" ] [ elt "pre" computation_tree ]
