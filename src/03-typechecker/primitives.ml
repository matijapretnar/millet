open Language

let poly_type ty =
  let a = Ast.TyParam.fresh "poly" in
  ([ a ], ty (Ast.TyParam a))

let integer_op_ty =
  ( [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.IntegerTy; Ast.TyConst Const.IntegerTy ],
        Ast.TyConst Const.BooleanTy ) )

let float_op_ty =
  ( [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.IntegerTy; Ast.TyConst Const.IntegerTy ],
        Ast.TyConst Const.BooleanTy ) )

let comparison_ty =
  poly_type (fun a ->
      Ast.TyArrow (Ast.TyTuple [ a; a ], Ast.TyConst Const.BooleanTy))

let primitive_type_scheme = function
  | Primitives.CompareEq -> comparison_ty
  | Primitives.CompareLt -> comparison_ty
  | Primitives.CompareGt -> comparison_ty
  | Primitives.CompareLe -> comparison_ty
  | Primitives.CompareGe -> comparison_ty
  | Primitives.CompareNe -> comparison_ty
  | Primitives.IntegerAdd -> integer_op_ty
  | Primitives.IntegerMul -> integer_op_ty
  | Primitives.IntegerSub -> integer_op_ty
  | Primitives.IntegerDiv -> integer_op_ty
  | Primitives.IntegerMod -> integer_op_ty
  | Primitives.IntegerNeg -> integer_op_ty
  | Primitives.FloatAdd -> float_op_ty
  | Primitives.FloatMul -> float_op_ty
  | Primitives.FloatSub -> float_op_ty
  | Primitives.FloatDiv -> float_op_ty
  | Primitives.FloatPow -> float_op_ty
  | Primitives.FloatNeg -> float_op_ty
  | Primitives.ToString ->
      poly_type (fun a -> Ast.TyArrow (a, Ast.TyConst Const.StringTy))
