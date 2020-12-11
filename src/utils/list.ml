include Stdlib.List

let fold_map f s xs =
  let aux (s, reversed_ys) x =
    let s', y = f s x in
    (s', y :: reversed_ys)
  in
  let s', reversed_ys = fold_left aux (s, []) xs in
  (s', rev reversed_ys)
