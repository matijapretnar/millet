include Interpreter

let view_run_state (run_state : Interpreter.run_state) dry =
  match (run_state, dry) with
  | { computations = Ast.Return exp :: _; _ }, false ->
      Format.printf "return %t@." (Ast.print_expression ~max_level:0 exp)
  | _ -> ()
