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
  Typing error (file "invalid/invalid_match_type.mlt", line 6, char 7):
  Cannot unify b = a list
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
  Syntax error (file "invalid/non_linear_pattern.mlt", line 3, char 10):
  Variable a defined multiple times.
  ======================================================================
  invalid/occurs_check.mlt
  ======================================================================
  Typing error (file "invalid/occurs_check.mlt", line 1, char 23):
  This expression has a type that would require an infinite type
  ======================================================================
  invalid/polymorphism_id_id.mlt
  ======================================================================
  Typing error (file "invalid/polymorphism_id_id.mlt", line 3, char 18):
  Cannot unify string = int
  ======================================================================
  invalid/self_reference_without_rec.mlt
  ======================================================================
  Syntax error (file "invalid/self_reference_without_rec.mlt", line 1, char 25):
  Unknown name --f--
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
  invalid/top_level_annotation_mismatch.mlt
  ======================================================================
  Typing error (file "invalid/top_level_annotation_mismatch.mlt", line 1, char 17):
  Cannot unify bool = int
  ======================================================================
  invalid/top_level_nested_impure.mlt
  ======================================================================
  Syntax error (file "invalid/top_level_nested_impure.mlt", line 1, char 7):
  Only pure expressions are allowed
  ======================================================================
  invalid/use_undefined_type.mlt
  ======================================================================
  Syntax error (file "invalid/use_undefined_type.mlt", line 1, char 19):
  Unknown name --bar--
  ======================================================================
  invalid/typechecker/bare_lambda.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/bare_lambda.mlt", line 6, char 10):
  Cannot infer the type of the pattern x; it needs a type annotation
  ======================================================================
  invalid/typechecker/checking_lambda_no_arrow_type.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/checking_lambda_no_arrow_type.mlt", line 6, char 6):
  This function is expected to have type int
  ======================================================================
  invalid/typechecker/checking_recursive_lambda_no_arrow_type.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/checking_recursive_lambda_no_arrow_type.mlt", line 6, char 11):
  This recursive function is expected to have type int
  ======================================================================
  invalid/typechecker/empty_match.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/empty_match.mlt", line 7, char 5):
  Cannot infer the type of a match expression with no cases; an annotation is required
  ======================================================================
  invalid/typechecker/invalid_type_constructor.mlt
  ======================================================================
  Typing error: Type list expects 1 arguments but got 2.
  ======================================================================
  invalid/typechecker/invalid_variant_argument.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/invalid_variant_argument.mlt", line 7, char 5):
  Variant optional argument mismatch
  ======================================================================
  invalid/typechecker/non_lambda_application.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/non_lambda_application.mlt", line 5, char 5):
  This expression of type int is not a function
  ======================================================================
  invalid/typechecker/occurs_check_failure.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/occurs_check_failure.mlt", line 8, char 25):
  This expression has a type that would require an infinite type
  ======================================================================
  invalid/typechecker/plain_type_mismatch.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/plain_type_mismatch.mlt", line 5, char 10):
  Cannot unify bool = int
  ======================================================================
  invalid/typechecker/recursive_lambda_no_annotation.mlt
  ======================================================================
  Syntax error (file "invalid/typechecker/recursive_lambda_no_annotation.mlt", line 6, char 19):
  An argument of the definition loop needs a type annotation
  ======================================================================
  invalid/typechecker/structural_type_mismatch.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/structural_type_mismatch.mlt", line 7, char 22):
  Cannot unify int × int = int list
  ======================================================================
  invalid/typechecker/top_level_rigid_type_param.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/top_level_rigid_type_param.mlt", line 6, char 24):
  Cannot unify α = int
  ======================================================================
  invalid/typechecker/tuple_against_non_tuple_type.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/tuple_against_non_tuple_type.mlt", line 5, char 22):
  This tuple is expected to have type int list
  ======================================================================
  invalid/typechecker/tuple_arity_mismatch.mlt
  ======================================================================
  Typing error (file "invalid/typechecker/tuple_arity_mismatch.mlt", line 7, char 10):
  Expected a tuple pattern of type int × int, got a 3-tuple
