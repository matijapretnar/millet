module Typed = TypedAst
module Ast = Language.Ast
open Utils
module Const = Language.Const

type ty_lbl_opt = Ast.ty * Ast.label option

type pattern = (pattern', ty_lbl_opt) Source.phrase

and pattern' =
  | PVar of Ast.variable
  | PAnnotated of pattern * Ast.ty
  | PAs of pattern * Ast.variable
  | PTuple of pattern list
  | PVariant of Ast.label * pattern option
  | PConst of Const.t
  | PNonbinding

type expression = (expression', ty_lbl_opt) Source.phrase

and expression' =
  | Var of Ast.variable
  | Const of Const.t
  | Annotated of expression * Ast.ty
  | Tuple of expression list
  | Variant of Ast.label * expression option
  | Lambda of abstraction
  | RecLambda of Ast.variable * abstraction

and computation = (computation', ty_lbl_opt) Source.phrase

and computation' =
  | Return of expression
  | Do of computation * abstraction
  | Match of expression * abstraction list
  | Apply of expression * expression list

and abstraction = (abstraction', ty_lbl_opt) Source.phrase
and abstraction' = pattern * computation

and command =
  | TyDef of (Ast.ty_param list * Ast.ty_name * Ast.ty_def) list
  | TopLet of Ast.variable * expression
  | TopDo of computation
