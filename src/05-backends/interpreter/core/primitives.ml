open Utils
module Ast = Language.Ast
module Const = Language.Const
module Primitives = Language.Primitives

(* Every primitive receives [head], which resolves the outermost constructor of
   a value (see [Interpreter.head]), and applies it to every level of the value
   that it inspects. *)

let return_const c = Ast.unlocated (Ast.Return (Ast.unlocated (Ast.Const c)))

let binary_function head f expr =
  match (head expr).Ast.it with
  | Ast.Tuple [ expr1; expr2 ] -> f expr1 expr2
  | _ -> Error.runtime "Pair expected but got %t" (Ast.print_expression expr)

let get_int head expr =
  match (head expr).Ast.it with
  | Ast.Const (Const.Integer n) -> n
  | _ -> Error.runtime "Integer expected but got %t" (Ast.print_expression expr)

let get_float head expr =
  match (head expr).Ast.it with
  | Ast.Const (Const.Float x) -> x
  | _ -> Error.runtime "Float expected but got %t" (Ast.print_expression expr)

let int_to_int f head expr =
  return_const (Const.Integer (f (get_int head expr)))

let int_int_to_int f head =
  binary_function head (fun expr1 expr2 ->
      return_const (Const.Integer (f (get_int head expr1) (get_int head expr2))))

let float_to_float f head expr =
  return_const (Const.Float (f (get_float head expr)))

let float_float_to_float f head =
  binary_function head (fun expr1 expr2 ->
      return_const
        (Const.Float (f (get_float head expr1) (get_float head expr2))))

let rec compare_values head expr1 expr2 =
  match ((head expr1).Ast.it, (head expr2).Ast.it) with
  | Ast.Const c1, Ast.Const c2 -> (
      match Const.compare c1 c2 with
      | Const.Less -> -1
      | Const.Equal -> 0
      | Const.Greater -> 1)
  | Ast.Tuple exprs1, Ast.Tuple exprs2 ->
      List.compare (compare_values head) exprs1 exprs2
  | Ast.Variant (lbl1, expr1), Ast.Variant (lbl2, expr2) ->
      let c = Ast.Label.compare lbl1 lbl2 in
      if c <> 0 then c else Option.compare (compare_values head) expr1 expr2
  | (Ast.Lambda _ | Ast.RecLambda _ | Ast.TopVar _), _ ->
      Error.runtime "Incomparable expression %t"
        (Ast.print_expression ~max_level:0 expr1)
  | _, _ ->
      Error.runtime "Incomparable expression %t"
        (Ast.print_expression ~max_level:0 expr2)

let comparison f head =
  binary_function head (fun expr1 expr2 ->
      return_const (Const.Boolean (f (compare_values head expr1 expr2) 0)))

(* A value with every level resolved, for printing. A function keeps the name
   under which it was given. *)
let rec value head expr =
  let expr' = head expr in
  match expr'.Ast.it with
  | Ast.Tuple exprs ->
      { expr' with it = Ast.Tuple (List.map (value head) exprs) }
  | Ast.Variant (lbl, arg) ->
      { expr' with it = Ast.Variant (lbl, Option.map (value head) arg) }
  | Ast.Lambda _ | Ast.RecLambda _ -> expr
  | _ -> expr'

let primitive_function = function
  | Primitives.CompareEq -> comparison ( = )
  | Primitives.CompareLt -> comparison ( < )
  | Primitives.CompareGt -> comparison ( > )
  | Primitives.CompareLe -> comparison ( <= )
  | Primitives.CompareGe -> comparison ( >= )
  | Primitives.CompareNe -> comparison ( <> )
  | Primitives.IntegerAdd -> int_int_to_int ( + )
  | Primitives.IntegerMul -> int_int_to_int ( * )
  | Primitives.IntegerSub -> int_int_to_int ( - )
  | Primitives.IntegerDiv -> int_int_to_int ( / )
  | Primitives.IntegerMod -> int_int_to_int ( mod )
  | Primitives.IntegerNeg -> int_to_int ( ~- )
  | Primitives.FloatAdd -> float_float_to_float ( +. )
  | Primitives.FloatMul -> float_float_to_float ( *. )
  | Primitives.FloatSub -> float_float_to_float ( -. )
  | Primitives.FloatDiv -> float_float_to_float ( /. )
  | Primitives.FloatPow -> float_float_to_float ( ** )
  | Primitives.FloatNeg -> float_to_float ( ~-. )
  | Primitives.ToString ->
      fun head expr ->
        return_const (Const.String (Ast.string_of_expression (value head expr)))
