(* Implementation based on:
 *  Robert Tarjan
 *  "Depth-first search and linear graph algorithms"
 *  SIAM Journal on Computing, 1(2), 1972
 *)

module Wasm = Wasm.Types
module IntSet = Set.Make (Int)

type vert = { mutable index : int; mutable low : int; mutable onstack : bool }

let sccs_of_subtypes (dta : Wasm.sub_type array) : IntSet.t list =
  let len = Array.length dta in
  if len = 0 then []
  else
    let info =
      Array.init len (fun _ -> { index = -1; low = -1; onstack = false })
    in
    let stack = Array.make len (-1) in
    let stack_top = ref 0 in
    let index = ref 0 in
    let sccs = ref [] in

    let rec connect x =
      stack.(!stack_top) <- x;
      incr stack_top;
      let v = info.(x) in
      v.onstack <- true;
      v.index <- !index;
      v.low <- !index;
      incr index;
      sub_type v dta.(x);
      if v.low = v.index then sccs := scc x IntSet.empty :: !sccs
    and scc x ys =
      decr stack_top;
      let y = stack.(!stack_top) in
      info.(y).onstack <- false;
      let ys' = IntSet.add y ys in
      if x = y then ys' else scc x ys'
    and sub_type v = function
      | Wasm.SubT (_, xs, st) ->
          List.iter (heap_type v) xs;
          str_type v st
    and str_type v = function
      | Wasm.DefStructT (Wasm.StructT fts) -> List.iter (field_type v) fts
      | Wasm.DefArrayT (Wasm.ArrayT ft) -> field_type v ft
      | Wasm.DefFuncT (Wasm.FuncT (vts1, vts2)) ->
          List.iter (value_type v) vts1;
          List.iter (value_type v) vts2
    and field_type v (Wasm.FieldT (_, st)) =
      match st with
      | Wasm.ValStorageT vt -> value_type v vt
      | Wasm.PackStorageT _ -> ()
    and value_type v = function
      | Wasm.RefT (_, ht) -> heap_type v ht
      | Wasm.NumT _ | Wasm.VecT _ | Wasm.BotT -> ()
    and heap_type v = function VarHT x' -> var_type v x' | _ -> ()
    and var_type v (x' : Wasm.var) =
      match x' with
      | Wasm.StatX x ->
          let w = info.(x) in
          if w.index = -1 then begin
            connect x;
            v.low <- min v.low w.low
          end
          else if w.onstack then v.low <- min v.low w.index
      | _ -> assert false
    in

    for x = 0 to len - 1 do
      if info.(x).index = -1 then connect x
    done;
    List.rev !sccs
