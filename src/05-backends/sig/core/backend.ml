module Ast = Language.Ast

module type S = sig
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

  type step_label

  type step = { label : step_label; next_state : unit -> run_state }

  val run : load_state -> run_state

  val steps : run_state -> step list
end
