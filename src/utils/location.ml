(** Source code locations *)

type t = { filename : string; line : int; column : int }

let print { filename; line; column } ppf =
  if String.length filename != 0 then
    Format.fprintf ppf "file %S, line %d, char %d" filename line column
  else Format.fprintf ppf "line %d, char %d" (line - 1) column

let of_lexeme position =
  let filename = position.Lexing.pos_fname
  and line = position.Lexing.pos_lnum
  and column = position.Lexing.pos_cnum - position.Lexing.pos_bol + 1 in
  { filename; line; column }

type 'a located = { it : 'a; at : t }

let add_loc ~loc it = { it; at = loc }
