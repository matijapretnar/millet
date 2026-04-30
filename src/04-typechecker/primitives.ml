module Ast = Language.Ast
module Const = Language.Const
module Primitives = Language.Primitives

let poly_type ty =
  let a = Ast.TyParam.fresh "poly" in
  ([ a ], ty (Ast.TyParam a))

let unary_integer_op_ty =
  ([], Ast.TyArrow (Ast.TyConst Const.IntegerTy, Ast.TyConst Const.IntegerTy))

let binary_integer_op_ty =
  ( [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.IntegerTy; Ast.TyConst Const.IntegerTy ],
        Ast.TyConst Const.IntegerTy ) )

let unary_float_op_ty =
  ([], Ast.TyArrow (Ast.TyConst Const.FloatTy, Ast.TyConst Const.FloatTy))

let binary_float_op_ty =
  ( [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.FloatTy; Ast.TyConst Const.FloatTy ],
        Ast.TyConst Const.FloatTy ) )

let comparison_int_ty =
  ( [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.IntegerTy; Ast.TyConst Const.IntegerTy ],
        Ast.TyConst Const.BooleanTy ) )

let comparison_float_ty =
  ( [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.FloatTy; Ast.TyConst Const.FloatTy ],
        Ast.TyConst Const.BooleanTy ) )

let primitive_type_scheme = function
  | Primitives.CompareIntEq -> comparison_int_ty
  | Primitives.CompareIntLt -> comparison_int_ty
  | Primitives.CompareIntGt -> comparison_int_ty
  | Primitives.CompareIntLe -> comparison_int_ty
  | Primitives.CompareIntGe -> comparison_int_ty
  | Primitives.CompareIntNe -> comparison_int_ty
  | Primitives.CompareFloatEq -> comparison_float_ty
  | Primitives.CompareFloatLt -> comparison_float_ty
  | Primitives.CompareFloatGt -> comparison_float_ty
  | Primitives.CompareFloatLe -> comparison_float_ty
  | Primitives.CompareFloatGe -> comparison_float_ty
  | Primitives.CompareFloatNe -> comparison_float_ty
  | Primitives.IntegerAdd -> binary_integer_op_ty
  | Primitives.IntegerMul -> binary_integer_op_ty
  | Primitives.IntegerSub -> binary_integer_op_ty
  | Primitives.IntegerDiv -> binary_integer_op_ty
  | Primitives.IntegerMod -> binary_integer_op_ty
  | Primitives.IntegerNeg -> unary_integer_op_ty
  | Primitives.FloatAdd -> binary_float_op_ty
  | Primitives.FloatMul -> binary_float_op_ty
  | Primitives.FloatSub -> binary_float_op_ty
  | Primitives.FloatDiv -> binary_float_op_ty
  | Primitives.FloatPow -> binary_float_op_ty
  | Primitives.FloatNeg -> unary_float_op_ty
  | Primitives.ToString ->
      poly_type (fun a -> Ast.TyArrow (a, Ast.TyConst Const.StringTy))
