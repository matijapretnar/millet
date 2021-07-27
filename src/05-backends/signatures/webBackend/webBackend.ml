module type S = sig
  include Backend.S

  val view_step : step -> step Vdom.vdom

  val view_run_state : run_state -> step option -> 'a Vdom.vdom
end
