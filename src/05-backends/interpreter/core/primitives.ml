open Utils
module Ast = Language.Ast
module Const = Language.Const
module Primitives = Language.Primitives

let binary_function f = function
  | Ast.Tuple [ expr1; expr2 ] -> f expr1 expr2
  | expr -> Error.runtime "Pair expected but got %t" (Ast.print_expression expr)

let get_int = function
  | Ast.Const (Const.Integer n) -> n
  | expr ->
      Error.runtime "Integer expected but got %t" (Ast.print_expression expr)

let get_float = function
  | Ast.Const (Const.Float n) -> n
  | expr ->
      Error.runtime "Float expected but got %t" (Ast.print_expression expr)

let int_to f expr =
  let n = get_int expr in
  f n

let int_int_to f expr =
  binary_function
    (fun expr1 expr2 ->
      let n1 = get_int expr1 in
      let n2 = get_int expr2 in
      f n1 n2)
    expr

let float_to f expr =
  let n = get_float expr in
  f n

let float_float_to f expr =
  binary_function
    (fun expr1 expr2 ->
      let n1 = get_float expr1 in
      let n2 = get_float expr2 in
      f n1 n2)
    expr

let int_to_int f =
  int_to (fun n -> Ast.Return (Ast.Const (Const.Integer (f n))))

let int_int_to_int f =
  int_int_to (fun n1 n2 -> Ast.Return (Ast.Const (Const.Integer (f n1 n2))))

let float_to_float f =
  float_to (fun n -> Ast.Return (Ast.Const (Const.Float (f n))))

let float_float_to_float f =
  float_float_to (fun n1 n2 -> Ast.Return (Ast.Const (Const.Float (f n1 n2))))

let rec comparable_expression = function
  | Ast.Var _ -> true
  | Const _ -> true
  | Annotated (e, _) -> comparable_expression e
  | Tuple es -> List.for_all comparable_expression es
  | Variant (_, e) -> Option.fold ~none:true ~some:comparable_expression e
  | Lambda _ -> false
  | RecLambda _ -> false

let comparison f =
  binary_function (fun e1 e2 ->
      if not (comparable_expression e1) then
        Error.runtime "Incomparable expression %t"
          (Ast.print_expression ~max_level:0 e1)
      else if not (comparable_expression e2) then
        Error.runtime "Incomparable expression %t"
          (Ast.print_expression ~max_level:0 e2)
      else Ast.Return (Ast.Const (Const.Boolean (f e1 e2))))

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
      fun expr ->
        Ast.Return (Ast.Const (Const.String (Ast.string_of_expression expr)))
