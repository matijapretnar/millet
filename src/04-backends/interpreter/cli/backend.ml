include Interpreter

let view_run_state (run_state : Interpreter.run_state) =
  Format.printf "%t@." (Ast.print_computation run_state.computation)
