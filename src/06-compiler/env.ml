module Source = Utils.Source
open Language.Ast

exception Clash of string

module Set = struct
  include Set.Make (String)

  let disjoint_add x s = if mem x s then raise (Clash x) else add x s
  let disjoint_union s1 s2 = fold disjoint_add s2 s1
end

type ('v, 't, 'f) env = {
  vals : ('v, unit) Source.phrase VariableMap.t;
  typs : ('t, unit) Source.phrase TyNameMap.t;
  prims : ('f, unit) Source.phrase VariableMap.t;
  runs : int;
}

let empty =
  {
    vals = VariableMap.empty;
    typs = TyNameMap.empty;
    prims = VariableMap.empty;
    runs = 0;
  }

let val_not_found x =
  print_endline "I am going to fail val :(";
  Variable.print x Format.str_formatter;
  failwith (Format.flush_str_formatter ())

let typ_not_found x =
  print_endline "type";
  TyName.print x Format.str_formatter;
  failwith (Format.flush_str_formatter ())

let lbl_not_found x =
  print_endline "label";
  Label.print x Format.str_formatter;
  failwith (Format.flush_str_formatter ())

let func_not_found x =
  print_endline "I am going to fail func :(";
  Variable.print x Format.str_formatter;
  failwith (Format.flush_str_formatter ())

let update_runs env = { env with runs = env.runs + 1 }
let get_runs env = env.runs
let extend_val env x v = { env with vals = VariableMap.add x v env.vals }
let extend_typ env x v = { env with typs = TyNameMap.add x v env.typs }
let extend_prim env x v = { env with prims = VariableMap.add x v env.prims }
let extend_vals env xs vs = List.fold_left2 extend_val env xs vs
let extend_typs env xs vs = List.fold_left2 extend_typ env xs vs
let extend_lbls env ys ts = List.fold_left2 extend_typ env ys ts
let extend_prims env xs vs = List.fold_left2 extend_prim env xs vs

let find_val x env =
  try VariableMap.find x env.vals with Not_found -> val_not_found x

let find_typ x env =
  try TyNameMap.find x env.typs with Not_found -> typ_not_found x

let find_prim x env =
  try VariableMap.find x env.prims with Not_found -> func_not_found x

let find_opt_val x env = VariableMap.find_opt x env.vals
let find_opt_typ x env = TyNameMap.find_opt x env.typs
let find_opt_prim x env = VariableMap.find_opt x env.prims
let iter_vals f env = VariableMap.iter f env.vals
