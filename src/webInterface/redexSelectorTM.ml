open Utils
module Ast = Core.Ast
module Interpreter = Core.Interpreter

let tag_marker = "###"

let print_mark ppf = Format.pp_print_as ppf 0 tag_marker

let print_computation_redex ?max_level red c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match (red, c) with
  | Interpreter.DoReturn, Ast.Do (c1, (pat, c2)) ->
      print "@[<hov>%tlet@[<hov>@ %t =@ %t@]%t in@ %t@]" print_mark
        (Ast.print_pattern pat) (Ast.print_computation c1) print_mark
        (Ast.print_computation c2)
  | _, comp ->
      print "%t%t%t" print_mark
        (fun ppf -> Ast.print_computation ?max_level comp ppf)
        print_mark

let rec print_computation_reduction ?max_level red c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match (red, c) with
  | Interpreter.DoCtx red, Ast.Do (c1, (Ast.PNonbinding, c2)) ->
      print "@[<hov>%t;@ %t@]"
        (print_computation_reduction red c1)
        (Ast.print_computation c2)
  | Interpreter.DoCtx red, Ast.Do (c1, (pat, c2)) ->
      print "@[<hov>let@[<hov>@ %t =@ %t@] in@ %t@]" (Ast.print_pattern pat)
        (print_computation_reduction red c1)
        (Ast.print_computation c2)
  | Interpreter.ComputationRedex redex, c ->
      print_computation_redex ?max_level redex c ppf
  | _, _ -> assert false

let split_string sep str =
  let sep_len = String.length sep and str_len = String.length str in
  let sub_start = ref 0 and sub_end = ref 0 and subs = ref [] in
  while !sub_end <= str_len - sep_len do
    if String.sub str !sub_end sep_len = sep then (
      subs := String.sub str !sub_start (!sub_end - !sub_start) :: !subs;
      sub_start := !sub_end + sep_len;
      sub_end := !sub_start )
    else incr sub_end
  done;
  if !sub_start <= str_len then
    subs := String.sub str !sub_start (str_len - !sub_start) :: !subs;
  List.rev !subs

let view_computation_with_redexes red comp =
  ( match red with
  | None -> Ast.print_computation comp Format.str_formatter
  | Some red -> print_computation_reduction red comp Format.str_formatter );
  match split_string tag_marker (Format.flush_str_formatter ()) with
  | [ code ] -> [ Vdom.text code ]
  | [ pre; redex; post ] ->
      [
        Vdom.text pre;
        Vdom.elt "strong" ~a:[ Vdom.class_ "has-text-info" ] [ Vdom.text redex ];
        Vdom.text post;
      ]
  | _ -> assert false
