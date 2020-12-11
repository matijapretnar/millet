open Utils
module Ast = Core.Ast
module Interpreter = Core.Interpreter

type snapshot = { computation : Ast.computation }

type loaded_code = {
  snapshot : snapshot;
  history : snapshot list;
  interpreter_state : Interpreter.state;
}

type model = {
  use_stdlib : bool;
  unparsed_code : string;
  loaded_code : (loaded_code, string) result;
  selected_reduction : int option;
  random_step_size : int;
}

type msg =
  | UseStdlib of bool
  | ChangeSource of string
  | LoadSource
  | EditSource
  | SelectReduction of int option
  | Step of Interpreter.top_step
  | RandomStep
  | ChangeRandomStepSize of int
  | Back

let init =
  {
    use_stdlib = true;
    unparsed_code = "";
    loaded_code = Error "";
    selected_reduction = None;
    random_step_size = 1;
  }

let step_snapshot = function Interpreter.Step comp' -> { computation = comp' }

let apply_to_code_if_loaded f model =
  match model.loaded_code with
  | Ok code -> { model with loaded_code = Ok (f code) }
  | Error _ -> model

let steps code =
  Interpreter.top_steps code.interpreter_state code.snapshot.computation

let move_to_snapshot snapshot code =
  { code with snapshot; history = code.snapshot :: code.history }

let step_code step code = move_to_snapshot (step_snapshot step) code

let rec make_random_steps num_steps code =
  match (num_steps, steps code) with
  | 0, _ | _, [] -> code
  | _, steps ->
      let i = Random.int (List.length steps) in
      let _, top_step = List.nth steps i in
      let code' = step_code top_step code in
      make_random_steps (num_steps - 1) code'

let parse_step_size input =
  input |> int_of_string_opt
  |> Option.to_result ~none:(input ^ " is not an integer")

let parse_source source =
  try
    let state = Loader.load_source Loader.initial_state source in
    let comp = Loader.make_computation state in
    Ok
      {
        snapshot = { computation = comp };
        history = [];
        interpreter_state = state.interpreter;
      }
  with Error.Error (_, _, msg) -> Error msg

let update model = function
  | UseStdlib use_stdlib -> { model with use_stdlib }
  | SelectReduction selected_reduction -> { model with selected_reduction }
  | Step top_step -> apply_to_code_if_loaded (step_code top_step) model
  | RandomStep ->
      apply_to_code_if_loaded (make_random_steps model.random_step_size) model
  | Back -> (
      match model.loaded_code with
      | Ok ({ history = snapshot' :: history'; _ } as code) ->
          {
            model with
            loaded_code =
              Ok { code with snapshot = snapshot'; history = history' };
          }
      | _ -> model )
  | ChangeSource input -> { model with unparsed_code = input }
  | LoadSource ->
      {
        model with
        loaded_code =
          parse_source
            ( (if model.use_stdlib then Loader.stdlib_source else "")
            ^ "\n\n\n" ^ model.unparsed_code );
      }
  | EditSource -> { model with loaded_code = Error "" }
  | ChangeRandomStepSize random_step_size -> { model with random_step_size }
