type primitive =
  | CompareEq
  | CompareLt
  | CompareGt
  | CompareLe
  | CompareGe
  | CompareNe
  | IntegerAdd
  | IntegerMul
  | IntegerSub
  | IntegerDiv
  | IntegerMod
  | IntegerNeg
  | FloatAdd
  | FloatMul
  | FloatSub
  | FloatDiv
  | FloatPow
  | FloatNeg
  | ToString

(* Keep this list up to date with the type above, otherwise the missing primitives will not be loaded *)
let primitives =
  [
    CompareEq;
    CompareLt;
    CompareGt;
    CompareLe;
    CompareGe;
    CompareNe;
    IntegerAdd;
    IntegerMul;
    IntegerSub;
    IntegerDiv;
    IntegerMod;
    IntegerNeg;
    FloatAdd;
    FloatMul;
    FloatSub;
    FloatDiv;
    FloatPow;
    FloatNeg;
    ToString;
  ]

let primitive_name = function
  | CompareEq -> "__compare_eq__"
  | CompareLt -> "__compare_lt__"
  | CompareGt -> "__compare_gt__"
  | CompareLe -> "__compare_le__"
  | CompareGe -> "__compare_ge__"
  | CompareNe -> "__compare_ne__"
  | IntegerAdd -> "__integer_add__"
  | IntegerMul -> "__integer_mul__"
  | IntegerSub -> "__integer_sub__"
  | IntegerDiv -> "__integer_div__"
  | IntegerMod -> "__integer_mod__"
  | IntegerNeg -> "__integer_neg__"
  | FloatAdd -> "__float_add__"
  | FloatMul -> "__float_mul__"
  | FloatSub -> "__float_sub__"
  | FloatDiv -> "__float_div__"
  | FloatPow -> "__float_pow__"
  | FloatNeg -> "__float_neg__"
  | ToString -> "__to_string__"
