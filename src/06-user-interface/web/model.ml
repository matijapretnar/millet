open Utils
module Ast = Language.Ast
module Backend = WebInterpreter
module Loader = Loader.Loader (Backend)

type edit_model = { use_stdlib : bool; unparsed_code : string }

let edit_init = { use_stdlib = true; unparsed_code = "" }

type edit_msg = UseStdlib of bool | ChangeSource of string

let edit_update edit_model = function
  | UseStdlib use_stdlib -> { edit_model with use_stdlib }
  | ChangeSource input -> { edit_model with unparsed_code = input }

type run_model = {
  run_state : Backend.run_state;
  history : Backend.run_state list;
  selected_step_index : int option;
  (* You may be wondering why we keep an index rather than the selected step itself.
     The selected step is displayed when the user moves the mouse over the step button,
     so on a onmouseover event. However, in the common case, when the user is on the button
     and is clicking it to proceed, this event is not triggered and so the step is not updated.
     For that reason, it is easiest to keep track of the selected button index, which does not
     change when the user clicks the button. *)
  random_step_size : int;
}

let run_init run_state =
  { run_state; history = []; selected_step_index = None; random_step_size = 1 }

type run_msg =
  | SelectStepIndex of int option
  | MakeStep of Backend.step
  | RandomStep
  | ChangeRandomStepSize of int
  | Back

let run_model_make_step run_model step =
  {
    run_model with
    run_state = Backend.step run_model.run_state step;
    history = run_model.run_state :: run_model.history;
  }

let rec run_model_make_random_steps run_model num_steps =
  match (num_steps, Backend.steps run_model.run_state) with
  | 0, _ | _, [] -> run_model
  | _, steps ->
      let i = Random.int (List.length steps) in
      let step = List.nth steps i in
      let run_model' = run_model_make_step run_model step in
      run_model_make_random_steps run_model' (num_steps - 1)

let run_update run_model = function
  | SelectStepIndex selected_step_index ->
      { run_model with selected_step_index }
  | MakeStep step -> run_model_make_step run_model step
  | RandomStep ->
      run_model_make_random_steps run_model run_model.random_step_size
  | Back -> (
      match run_model.history with
      | run_state' :: history' ->
          { run_model with run_state = run_state'; history = history' }
      | _ -> run_model )
  | ChangeRandomStepSize random_step_size -> { run_model with random_step_size }

type model = { edit_model : edit_model; run_model : (run_model, string) result }

let init = { edit_model = edit_init; run_model = Error "" }

type msg = EditMsg of edit_msg | RunCode | RunMsg of run_msg | EditCode

let update model = function
  | EditMsg edit_msg ->
      { model with edit_model = edit_update model.edit_model edit_msg }
  | RunMsg run_msg -> (
      match model.run_model with
      | Ok run_model ->
          { model with run_model = Ok (run_update run_model run_msg) }
      | Error _ -> model )
  | RunCode ->
      let run_model =
        try
          let source =
            (if model.edit_model.use_stdlib then Loader.stdlib_source else "")
            ^ "\n\n\n" ^ model.edit_model.unparsed_code
          in
          let state = Loader.load_source Loader.initial_state source in
          let run_state = Backend.run state.backend in
          let run_model = run_init run_state in
          Ok run_model
        with Error.Error (_, _, msg) -> Error msg
      in
      { model with run_model }
  | EditCode -> { model with run_model = Error "" }
