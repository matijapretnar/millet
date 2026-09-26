  $ for f in invalid/*.mlt invalid/typechecker/*.mlt
  > do
  >   echo "======================================================================"
  >   echo $f
  >   echo "======================================================================"
  >   ../cli.exe $f
  >   : # this command is here to suppress potential non-zero exit codes in the output
  > done
  ======================================================================
  invalid/duplicate_variant_tydef_sum.mlt
  ======================================================================
  Syntax error (file "invalid/duplicate_variant_tydef_sum.mlt", line 3, char 1):
  Label Horn defined multiple times.
  ======================================================================
  invalid/invalid_match_type.mlt
  ======================================================================
  Typing error: Cannot unify a list = b
  ======================================================================
  invalid/less_than_function.mlt
  ======================================================================
  Runtime error: Incomparable expression (fun x ↦ return x)
  ======================================================================
  invalid/malformed_type_application.mlt
  ======================================================================
  Typing error: Type foo expects 1 arguments but got 2.
  ======================================================================
  invalid/non_linear_pattern.mlt
  ======================================================================
  Syntax error (file "invalid/non_linear_pattern.mlt", line 3, char 9):
  Variable a defined multiple times.
  ======================================================================
  invalid/occurs_check.mlt
  ======================================================================
  Typing error: This expression has a type that would require an infinite type
  ======================================================================
  invalid/polymorphism_id_id.mlt
  ======================================================================
  Typing error: Cannot unify string = int
  ======================================================================
  invalid/shadow_label.mlt
  ======================================================================
  Syntax error (file "invalid/shadow_label.mlt", line 2, char 1):
  Label Horn defined multiple times.
  ======================================================================
  invalid/shadow_type.mlt
  ======================================================================
  Syntax error (file "invalid/shadow_type.mlt", line 3, char 1):
  Type cow defined multiple times.
  ======================================================================
  invalid/use_undefined_type.mlt
  ======================================================================
  Syntax error (file "invalid/use_undefined_type.mlt", line 1, char 19):
  Unknown name --bar--
  ======================================================================
  invalid/typechecker/bare_lambda.mlt
  ======================================================================
  Typing error: Cannot infer the type of a function; its argument pattern x needs a type annotation (or it needs to be used where an expected type is known)
  ======================================================================
  invalid/typechecker/checking_lambda_no_arrow_type.mlt
  ======================================================================
  Typing error: This function is expected to have type int
  ======================================================================
  invalid/typechecker/checking_recursive_lambda_no_arrow_type.mlt
  ======================================================================
  Typing error: This recursive function is expected to have type int
  ======================================================================
  invalid/typechecker/empty_match.mlt
  ======================================================================
  Typing error: Cannot infer the type of a match expression with no cases; an annotation is required
  ======================================================================
  invalid/typechecker/invalid_type_constructor.mlt
  ======================================================================
  Typing error: Type list expects 1 arguments but got 2.
  ======================================================================
  invalid/typechecker/invalid_variant_argument.mlt
  ======================================================================
  Typing error: Variant optional argument mismatch
  ======================================================================
  invalid/typechecker/non_lambda_application.mlt
  ======================================================================
  Typing error: This expression of type int is not a function
  ======================================================================
  invalid/typechecker/occurs_check_failure.mlt
  ======================================================================
  Typing error: This expression has a type that would require an infinite type
  ======================================================================
  invalid/typechecker/plain_type_mismatch.mlt
  ======================================================================
  Typing error: Cannot unify bool = int
  ======================================================================
  invalid/typechecker/recursive_lambda_no_annotation.mlt
  ======================================================================
  Typing error: Cannot infer the type of a recursive function (bound as loop); its argument pattern x needs a type annotation (or it needs to be used where an expected type is known)
  ======================================================================
  invalid/typechecker/structural_type_mismatch.mlt
  ======================================================================
  Typing error: Cannot unify int × int = int list
  ======================================================================
  invalid/typechecker/top_level_rigid_type_param.mlt
  ======================================================================
  Typing error: Cannot unify α = int
  ======================================================================
  invalid/typechecker/tuple_arity_mismatch.mlt
  ======================================================================
  Typing error: Expected a tuple pattern of type int × int, got a 3-tuple
