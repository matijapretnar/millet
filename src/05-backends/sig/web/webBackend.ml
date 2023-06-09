module type S = sig
  include Backend.S

  type msg
  type model

  val init : model
  val update_model : model -> msg -> model
  val update_run_state : run_state -> msg -> run_state
  val view_model : model -> msg Vdom.vdom
  val view_step_label : step_label -> 'a Vdom.vdom
  val view_run_state : model -> run_state -> step_label option -> 'a Vdom.vdom
end
