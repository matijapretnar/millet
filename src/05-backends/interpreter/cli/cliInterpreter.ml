include Interpreter

let view_run_state (run_state : Interpreter.run_state) =
  match run_state with
  | { computations = { it = Ast.Return exp; _ } :: _; environment } ->
      Format.printf "return %t@."
        (Ast.print_expression ~max_level:0 (value environment exp))
  | _ -> ()
