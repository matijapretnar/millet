include Stdlib.List

let fold = fold_left

let fold_map f s xs =
  let aux (s, reversed_ys) x =
    let s', y = f s x in
    (s', y :: reversed_ys)
  in
  let s', reversed_ys = fold aux (s, []) xs in
  (s', rev reversed_ys)

let rec left_to_right_map f = function
  | [] -> []
  | x :: xs ->
      let y = f x in
      let ys = left_to_right_map f xs in
      y :: ys

let concat_map f xs = concat (map f xs)

let unique_elements lst =
  let rec unique_elements acc = function
    | [] -> rev acc
    | x :: xs ->
        if mem x acc then unique_elements acc xs
        else unique_elements (x :: acc) xs
  in
  unique_elements [] lst

let no_duplicates lst =
  let rec check seen = function
    | [] -> true
    | x :: xs -> (not (mem x seen)) && check (x :: seen) xs
  in
  check [] lst

let list_diff lst1 lst2 = filter (fun x -> not (mem x lst2)) lst1
