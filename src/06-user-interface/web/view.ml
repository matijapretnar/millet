open Vdom
module Ast = Language.Ast

(* Auxiliary definitions *)
let panel ?(a = []) heading blocks =
  div ~a:(class_ "panel" :: a)
    (elt "p" ~a:[ class_ "panel-heading" ] [ text heading ] :: blocks)

let panel_block = div ~a:[ class_ "panel-block" ]

let button txt msg =
  input [] ~a:[ onclick (fun _ -> msg); type_button; value txt ]

let disabled_button txt = input [] ~a:[ type_button; value txt; disabled true ]

let select ?(a = []) empty_description msg describe_choice selected choices =
  let view_choice choice =
    elt "option"
      ~a:[ bool_prop "selected" (selected choice) ]
      [ text (describe_choice choice) ]
  in
  div ~a
    [
      elt "select"
        ~a:[ onchange_index (fun i -> msg (List.nth choices (i - 1))) ]
        (elt "option"
           ~a:
             [
               disabled true;
               bool_prop "selected"
                 (List.for_all (fun choice -> not (selected choice)) choices);
             ]
           [ text empty_description ]
        :: List.map view_choice choices);
    ]

let nil = text ""

let view_contents main aside =
  div
    ~a:[ class_ "contents columns" ]
    [
      div ~a:[ class_ "main column is-three-quarters" ] main;
      div ~a:[ class_ "aside column is-one-quarter" ] aside;
    ]

(* Edit view *)

let view_editor (model : Model.edit_model) =
  div
    ~a:[ class_ "box" ]
    [
      elt "textarea"
        ~a:
          [
            class_ "textarea has-fixed-size";
            oninput (fun input -> Model.ChangeSource input);
            int_prop "rows"
              (max 10
                 (String.split_on_char '\n' model.unparsed_code |> List.length));
          ]
        [ text model.unparsed_code ];
    ]

(* let _view (model : Model.model) =
   match model.loaded_code with
   | Ok code ->
       div
         [
           input ~a:[type_ "range"; int_attr "min" 0; int_attr "max" 10; int_attr "step" 2; onmousedown (fun event -> Model.ParseInterrupt (string_of_int event.x))] [];
           (* elt "progress" ~a:[type_ "range"; value (string_of_int model.random_step_size); int_attr "max" 10; oninput (fun input -> Model.ChangeStepSize input)] []; *)
           editor model;
           actions model code;
           view_operations code.snapshot.operations;
           view_process code.snapshot.process;
         ]
   | Error msg -> div [ editor model; text msg ] *)

let view_compiler (model : Model.model) =
  let use_stdlib =
    elt "label"
      ~a:[ class_ "panel-block" ]
      [
        input
          ~a:
            [
              type_ "checkbox";
              onchange_checked (fun use_stdlib ->
                  Model.EditMsg (Model.UseStdlib use_stdlib));
              bool_prop "checked" model.edit_model.use_stdlib;
            ]
          [];
        text "Load standard library";
      ]
  in
  let load_example =
    div
      ~a:[ class_ "panel-block" ]
      [
        div
          ~a:[ class_ "field" ]
          [
            div
              ~a:[ class_ "control is-expanded" ]
              [
                select
                  ~a:[ class_ "select is-fullwidth" ]
                  "Load example"
                  (fun (_, source) -> Model.EditMsg (ChangeSource source))
                  (fun (title, _) -> title)
                  (fun _ -> false)
                  (* The module Examples_mlt is semi-automatically generated from examples/*.mlt. Check the dune file for details. *)
                  Examples_mlt.examples;
              ];
          ];
      ]
  and run_process =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-info is-fullwidth";
              onclick (fun _ -> Model.RunCode);
              (* disabled (Result.is_error model.loaded_code); *)
            ]
          [ text "Compile & run" ];
        (match model.run_model with
        | Error msg -> elt "p" ~a:[ class_ "help is-danger" ] [ text msg ]
        | Ok _ -> nil);
      ]
  in
  panel "Code options" [ use_stdlib; load_example; run_process ]

let edit_view (model : Model.model) =
  view_contents
    [
      map
        (fun edit_msg -> Model.EditMsg edit_msg)
        (view_editor model.edit_model);
    ]
    [ view_compiler model ]

(* Run view *)

let view_steps (run_model : Model.run_model) steps =
  let view_edit_source =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-outlined is-fullwidth is-small is-danger";
              onclick (fun _ -> Model.EditCode);
              attr "title"
                "Re-editing source code will abort current evaluation";
            ]
          [ text "Re-edit source code" ];
      ]
  and view_undo_last_step =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-outlined is-fullwidth is-small";
              onclick (fun _ -> Model.RunMsg Model.Back);
              disabled (run_model.history = []);
            ]
          [ text "Undo last step" ];
      ]
  and view_step i step =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-outlined is-fullwidth";
              onclick (fun _ -> Model.RunMsg (Model.MakeStep step));
              onmousemove (fun _ ->
                  Model.RunMsg (Model.SelectStepIndex (Some i)));
            ]
          [
            Vdom.map
              (fun step -> Model.RunMsg (Model.MakeStep step))
              (Model.Backend.view_step_label step.label);
          ];
      ]
  and view_random_steps steps =
    div
      ~a:[ class_ "panel-block" ]
      [
        div
          ~a:[ class_ "field has-addons" ]
          [
            div
              ~a:[ class_ "control is-expanded" ]
              [
                select
                  ~a:[ class_ "select is-fullwidth is-info" ]
                  "Step size"
                  (fun step_size ->
                    Model.RunMsg (Model.ChangeRandomStepSize step_size))
                  string_of_int
                  (fun step_size -> step_size = run_model.random_step_size)
                  [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024 ];
              ];
            div
              ~a:[ class_ "control" ]
              [
                elt "button"
                  ~a:
                    [
                      class_ "button is-info";
                      onclick (fun _ -> Model.RunMsg Model.RandomStep);
                      disabled (steps = []);
                    ]
                  [ text "random steps" ];
              ];
          ];
        (if steps = [] then
         elt "p"
           ~a:[ class_ "help" ]
           [ text "Computation has terminated, no further steps are possible." ]
        else text "");
      ]
  in
  panel "Interaction"
    ~a:[ onmousemove (fun _ -> Model.RunMsg (Model.SelectStepIndex None)) ]
    (view_edit_source :: view_undo_last_step :: view_random_steps steps
   :: List.mapi view_step steps)

let run_view (run_model : Model.run_model) =
  let steps = Model.Backend.steps run_model.run_state in
  let selected_step =
    Option.map (List.nth steps) run_model.selected_step_index
  in
  view_contents
    [
      Model.Backend.view_run_state run_model.run_state
        (Option.map (fun step -> step.WebInterpreter.label) selected_step);
    ]
    [ view_steps run_model steps ]

let view_navbar =
  let view_title =
    div
      ~a:[ class_ "navbar-brand" ]
      [
        elt "a"
          ~a:[ class_ "navbar-item" ]
          [ elt "p" ~a:[ class_ "title" ] [ text "Millet" ] ];
      ]
  in

  elt "navbar" ~a:[ class_ "navbar" ] [ view_title ]

let view (model : Model.model) =
  div
    [
      view_navbar;
      (match model.run_model with
      | Error _ -> edit_view model
      | Ok run_model -> run_view run_model);
    ]
