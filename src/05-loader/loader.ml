open Utils
module Ast = Language.Ast

let parse_commands lexbuf =
  try Parser.Grammar.commands Parser.Lexer.token lexbuf with
  | Parser.Grammar.Error ->
      Error.syntax
        ~loc:(Location.of_lexeme (Lexing.lexeme_start_p lexbuf))
        "parser error"
  | Failure failmsg when failmsg = "lexing: empty token" ->
      Error.syntax
        ~loc:(Location.of_lexeme (Lexing.lexeme_start_p lexbuf))
        "unrecognised symbol."

type state = {
  desugarer : Parser.Desugarer.state;
  interpreter : Interpreter.state;
  typechecker : Typechecker.state;
  top_computations : Ast.computation list;
}

let execute_command state = function
  | Ast.TyDef ty_defs ->
      let typechecker_state' =
        Typechecker.add_type_definitions state.typechecker ty_defs
      in
      { state with typechecker = typechecker_state' }
  | Ast.TopLet (x, expr) ->
      let interpreter_state' =
        Interpreter.eval_top_let state.interpreter x expr
      in
      let typechecker_state' =
        Typechecker.add_top_definition state.typechecker x expr
      in
      {
        state with
        interpreter = interpreter_state';
        typechecker = typechecker_state';
      }
  | Ast.TopDo comp ->
      let _ = Typechecker.infer state.typechecker comp in
      { state with top_computations = comp :: state.top_computations }

let load_commands state cmds =
  let desugarer_state', cmds' =
    List.fold_map Parser.Desugarer.desugar_command state.desugarer cmds
  in
  let state' = { state with desugarer = desugarer_state' } in
  List.fold_left execute_command state' cmds'

let load_source state source =
  let lexbuf = Lexing.from_string source in
  let cmds = parse_commands lexbuf in
  load_commands state cmds

let load_file state source =
  let cmds = Parser.Lexer.read_file parse_commands source in
  load_commands state cmds

let make_computation state =
  match state.top_computations with
  | [] -> Ast.Return (Ast.Tuple [])
  | comp :: _ -> comp

(** The module Stdlib_mlt is automatically generated from stdlib.mlt. Check the dune file for details. *)
let stdlib_source = Stdlib_mlt.contents

let load_primitive state prim =
  let x = Ast.Variable.fresh (Language.Primitives.primitive_name prim) in
  let desugarer_state' =
    Parser.Desugarer.load_primitive state.desugarer x prim
  in
  let typechecker_state' =
    Typechecker.load_primitive state.typechecker x prim
  in
  let interpreter_state' =
    Interpreter.load_primitive state.interpreter x prim
  in
  {
    state with
    desugarer = desugarer_state';
    interpreter = interpreter_state';
    typechecker = typechecker_state';
  }

let initial_state =
  let initial_state_without_primitives =
    {
      desugarer = Parser.Desugarer.initial_state;
      interpreter = Interpreter.initial_state;
      typechecker = Typechecker.initial_state;
      top_computations = [];
    }
  in

  List.fold_left load_primitive initial_state_without_primitives
    Language.Primitives.primitives
