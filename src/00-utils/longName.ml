module LongName = struct
  type t = string list * string

  let create (prefix : string list) (id : string) : t = (prefix, id)
  let from_id (id : string) : t = create [] id

  (* TODO: Refactor the following list. Do not use List.rev.*)
  let from_list (l : string list) : t =
    let rl = List.rev l in
    let id = List.hd rl in
    let prefix = List.rev (List.tl rl) in
    create prefix id

  let prefix (p : t) : string list =
    let prefix, _ = p in
    prefix

  let id (p : t) : string =
    let _, id = p in
    id

  let with_prefix (p : t) (prf : string list) : t =
    let prf' = prefix p in
    let id' = id p in
    create (prf @ prf') id'

  let to_list (p : t) : string list = prefix p @ [ id p ]

  let compare (p1 : t) (p2 : t) : int =
    let pf1 = prefix p1 in
    let pf2 = prefix p2 in
    let r = List.compare_lengths pf1 pf2 in
    if r != 0 then r
    else
      let zp = List.combine pf1 pf2 @ [ (id p1, id p2) ] in
      List.fold_left
        (fun r (s1, s2) -> if r != 0 then r else String.compare s1 s2)
        0 zp

  let to_string ?(sdot = ".") (p : t) : string =
    String.concat sdot (prefix p @ [ id p ])

  let rec pop = function [] -> [] | [ _ ] -> [] | hd :: ls -> hd :: pop ls
end
