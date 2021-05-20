open Utils
module Ast = Language.Ast

module type Backend = sig
  type load_state

  val initial_load_state : load_state

  val load_primitive :
    load_state -> Ast.variable -> Language.Primitives.primitive -> load_state

  val load_ty_def :
    load_state ->
    (Ast.ty_param list * Ast.ty_name * Ast.ty_def) list ->
    load_state

  val load_top_let : load_state -> Ast.variable -> Ast.expression -> load_state

  val load_top_do : load_state -> Ast.computation -> load_state

  type run_state

  type step

  val run : load_state -> run_state

  val steps : run_state -> step list

  val step : run_state -> step -> run_state
end

module Loader (Backend : Backend) = struct
  type state = {
    desugarer : Desugarer.state;
    backend : Backend.load_state;
    typechecker : Typechecker.state;
  }

  let load_primitive state prim =
    let x = Ast.Variable.fresh (Language.Primitives.primitive_name prim) in
    let desugarer_state' = Desugarer.load_primitive state.desugarer x prim in
    let typechecker_state' =
      Typechecker.load_primitive state.typechecker x prim
    in
    let backend_state' = Backend.load_primitive state.backend x prim in
    {
      desugarer = desugarer_state';
      typechecker = typechecker_state';
      backend = backend_state';
    }

  let initial_state =
    let initial_state_without_primitives =
      {
        desugarer = Desugarer.initial_state;
        typechecker = Typechecker.initial_state;
        backend = Backend.initial_load_state;
      }
    in

    List.fold_left load_primitive initial_state_without_primitives
      Language.Primitives.primitives

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

  let execute_command state = function
    | Ast.TyDef ty_defs ->
        let typechecker_state' =
          Typechecker.add_type_definitions state.typechecker ty_defs
        in
        let backend_state' = Backend.load_ty_def state.backend ty_defs in
        {
          state with
          typechecker = typechecker_state';
          backend = backend_state';
        }
    | Ast.TopLet (x, expr) ->
        let typechecker_state' =
          Typechecker.add_top_definition state.typechecker x expr
        in
        let backend_state' = Backend.load_top_let state.backend x expr in
        {
          state with
          typechecker = typechecker_state';
          backend = backend_state';
        }
    | Ast.TopDo comp ->
        let _ = Typechecker.infer state.typechecker comp in
        let backend_state' = Backend.load_top_do state.backend comp in
        { state with backend = backend_state' }

  let load_commands state cmds =
    let desugarer_state', cmds' =
      List.fold_map Desugarer.desugar_command state.desugarer cmds
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

  (** The module Stdlib_mlt is automatically generated from stdlib.mlt. Check the dune file for details. *)
  let stdlib_source = Stdlib_mlt.contents
end
