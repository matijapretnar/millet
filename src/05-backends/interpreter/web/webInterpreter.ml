include Interpreter
open Vdom

type msg = unit
type model = unit

let init = ()
let update () () = ()
let view_model () = Vdom.text ""

let view_computation_redex = function
  | Interpreter.Match -> "match"
  | Interpreter.ApplyFun -> "applyFun"
  | Interpreter.DoReturn -> "doReturn"

let rec view_computation_reduction = function
  | Interpreter.DoCtx red -> view_computation_reduction red
  | Interpreter.ComputationRedex redex -> view_computation_redex redex

let view_step_label = function
  | Interpreter.ComputationReduction reduction ->
      text (view_computation_reduction reduction)
  | Interpreter.Return -> text "return"

let view_run_state (run_state : run_state) step_label =
  match run_state with
  | { computations = comp :: _; _ } ->
      let reduction =
        match step_label with
        | Some (ComputationReduction red) -> Some red
        | Some Interpreter.Return -> None
        | None -> None
      in

      let computation_tree =
        RedexSelectorTM.view_computation_with_redexes reduction comp
      in
      div ~a:[ class_ "box" ] [ elt "pre" computation_tree ]
  | { computations = []; _ } ->
      div ~a:[ class_ "box" ] [ elt "pre" [ text "done" ] ]
