include Interpreter

let view_run_state (run_state : Interpreter.run_state) =
  match run_state with
  | { computations = Ast.Return exp :: _; _ } ->
      Format.printf "return %t@." (Ast.print_expression ~max_level:0 exp)
  | _ -> ()
