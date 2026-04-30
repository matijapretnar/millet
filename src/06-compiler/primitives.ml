module Ast = Language.Ast
module Const = Language.Const
module Primitives = Language.Primitives
module Emit = Emit
module Types = Wasm.Types
open Wasm.Ast

let simple_primitive_function ctxt prim =
  let emit ctxt = List.iter (Emit.emit_instr ctxt) in
  match prim with
  | Primitives.IntegerAdd -> emit ctxt [ Add I32T ]
  | Primitives.IntegerMul -> emit ctxt [ Mul I32T ]
  | Primitives.IntegerSub -> emit ctxt [ Sub I32T ]
  | Primitives.IntegerDiv -> emit ctxt [ DivS I32T ]
  | Primitives.IntegerMod -> emit ctxt [ Rem I32T ]
  | Primitives.IntegerNeg -> emit ctxt [ IntConst (I32T, -1); Mul I32T ]
  | Primitives.FloatAdd -> emit ctxt [ Add F64T ]
  | Primitives.FloatMul -> emit ctxt [ Mul F64T ]
  | Primitives.FloatSub -> emit ctxt [ Sub F64T ]
  | Primitives.FloatDiv -> emit ctxt [ Div F64T ]
  | Primitives.FloatPow -> emit ctxt [ Add F64T ]
  | Primitives.FloatNeg -> emit ctxt [ Neg F64T ]
  | Primitives.ToString -> failwith "implemented elsewhere"
  | Primitives.CompareIntEq -> emit ctxt [ Eq I32T ]
  | Primitives.CompareIntLt -> emit ctxt [ Lt I32T ]
  | Primitives.CompareIntGt -> emit ctxt [ Gt I32T ]
  | Primitives.CompareIntLe -> emit ctxt [ Le I32T ]
  | Primitives.CompareIntGe -> emit ctxt [ Ge I32T ]
  | Primitives.CompareIntNe -> emit ctxt [ Ne I32T ]
  | Primitives.CompareFloatEq -> emit ctxt [ Eq F64T ]
  | Primitives.CompareFloatLt -> emit ctxt [ Lt F64T ]
  | Primitives.CompareFloatGt -> emit ctxt [ Gt F64T ]
  | Primitives.CompareFloatLe -> emit ctxt [ Le F64T ]
  | Primitives.CompareFloatGe -> emit ctxt [ Ge F64T ]
  | Primitives.CompareFloatNe -> emit ctxt [ Ne F64T ]
