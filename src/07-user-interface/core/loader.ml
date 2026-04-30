open Utils
module Ast = Language.Ast
module Compiler = Compiler
module Typed = Typechecker.TypedAst

module Loader (Backend : Backend.S) = struct
  type state = {
    desugarer : Desugarer.state;
    backend : Backend.load_state;
    typechecker : Typechecker.state;
    compiler : Compiler.comp_state;
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
      compiler =
        { state.compiler with prims = (x, prim) :: state.compiler.prims };
    }

  let initial_state =
    let initial_state_without_primitives =
      {
        desugarer = Desugarer.initial_state;
        typechecker = Typechecker.initial_state;
        backend = Backend.initial_load_state;
        compiler = { cmds = []; prims = [] };
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
        let typechecker_state', cmd =
          Typechecker.add_top_definition state.typechecker x expr
        in
        let backend_state' = Backend.load_top_let state.backend x expr in
        {
          state with
          typechecker = typechecker_state';
          backend = backend_state';
          compiler =
            { state.compiler with cmds = state.compiler.cmds @ [ cmd ] };
        }
    | Ast.TopDo comp ->
        let _, cmd = Typechecker.infer state.typechecker comp in
        let backend_state' = Backend.load_top_do state.backend comp in
        {
          state with
          backend = backend_state';
          compiler =
            { state.compiler with cmds = state.compiler.cmds @ [ TopDo cmd ] };
        }

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

  let write_to_file file prog =
    let name =
      (Filename.basename file |> String.split_on_char '.' |> List.hd) ^ ".wat"
    in
    let oc = open_out name in
    (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\n" prog;
    (* write something *)
    close_out oc;

    (* flush and close the channel *)

    (* Read file and display the first line *)
    let ic = open_in file in
    try
      (* write the result to stdout *)
      flush stdout;
      (* write on the underlying device now *)
      close_in ic
      (* close the input channel *)
    with e ->
      (* some unexpected exception occurs *)
      close_in_noerr ic;
      (* emergency closing *)
      raise e

  (** The module Stdlib_mlt is automatically generated from stdlib.mlt. Check
      the dune file for details. *)
  let stdlib_source = Stdlib_mlt.contents
end
