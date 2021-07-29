  $ for f in *.mlt
  > do
  >   echo "======================================================================"
  >   echo $f
  >   echo "======================================================================"
  >   ../cli.exe $f
  >   :  # this command is here to suppress potential non-zero exit codes in the output
  > done
  ======================================================================
  duplicate_variant_tydef_sum.mlt
  ======================================================================
  Syntax error (file "duplicate_variant_tydef_sum.mlt", line 3, char 1):
  Label Horn defined multiple times.
  ======================================================================
  invalid_match_type.mlt
  ======================================================================
  Typing error: Cannot unify a list = b
  ======================================================================
  less_than_function.mlt
  ======================================================================
  Runtime error: Incomparable expression (fun x ↦ return x)
  ======================================================================
  lexer.mlt
  ======================================================================
  return 10
  return 20
  return 30
  return 40
  return -1000000000
  return 42
  return -42
  return 42
  return 42
  return 11259375
  return 11259375
  return 32072
  return 32072
  return 3.141592
  return 4.141592
  return -5.1592
  return 6.1592
  return -3.14
  ======================================================================
  malformed_type_application.mlt
  ======================================================================
  Type foo applied to too few arguments.
  ======================================================================
  nat.mlt
  ======================================================================
  return 42
  ======================================================================
  non_linear_pattern.mlt
  ======================================================================
  All the identifiers in a pattern should be unique.
  ======================================================================
  occurs_check.mlt
  ======================================================================
  Typing error: Cannot unify α = β → α
  ======================================================================
  orelse_andalso.mlt
  ======================================================================
  ======================================================================
  patterns.mlt
  ======================================================================
  return 5
  return (1, 2)
  return (1, 2::3::4::[])
  return (2::3::4::[])
  return 10
  return (10, Moo 10)
  return (42, 42, 42)
  return (1, 2, 3, (1, 2, 3))
  return ("foo", "foo", "bar")
  ======================================================================
  polymorphism.mlt
  ======================================================================
  return (5, "foo")
  return (4, "foo")
  return (1::u, "foo"::u)
  return ([]::v, (2::[])::v)
  return (fun x ↦ let h = return (fun t ↦ return (fun u ↦ return u)) in
                    let b = h x in b x)
  return (fun x ↦ let h = return (fun t ↦ return (fun u ↦ return t)) in
                    let b = h x in b x)
  ======================================================================
  polymorphism_id_id.mlt
  ======================================================================
  Typing error: Cannot unify int = string
  ======================================================================
  recursion.mlt
  ======================================================================
  return 5
  ======================================================================
  shadow_label.mlt
  ======================================================================
  Syntax error (file "shadow_label.mlt", line 2, char 1):
  Label Horn defined multiple times.
  ======================================================================
  shadow_type.mlt
  ======================================================================
  Syntax error (file "shadow_type.mlt", line 3, char 1):
  Type cow defined multiple times.
  ======================================================================
  test_equality.mlt
  ======================================================================
  return true
  return false
  return true
  return false
  return false
  return true
  ======================================================================
  test_less_then.mlt
  ======================================================================
  return false
  return true
  return false
  return false
  return true
  return false
  return false
  return "composite values"
  return true
  return false
  ======================================================================
  test_precedence_and_associativity.mlt
  ======================================================================
  return 1
  return 2
  return 5
  return 1
  return 5
  return 3
  return 27.
  return true
  return 22
  ======================================================================
  test_stdlib.mlt
  ======================================================================
  return "test less"
  return true
  return false
  return false
  return "test equal"
  return true
  return true
  return "test tilda_minus"
  return -1
  return -3.14159
  return -1.
  return "test integer operations"
  return 4
  return 4
  return 19
  return 65
  return 33
  return 0
  return 2
  return 0
  return "test float operations"
  return 8.
  return 5.84
  return 8.478
  return 0.44
  return 1.16296296296
  return infinity
  return "13"
  return "(1, 2, 3)::[]"
  return "(1, 2, 3)"
  return "fun x \226\134\166 return x"
  return "test some and none"
  return None
  return (Some 3)
  return "test ignore"
  return ()
  return "test not"
  return false
  return "test compare"
  return true
  return true
  return true
  return true
  return true
  return "test range"
  return (4::5::6::7::8::9::[])
  return "test map"
  return (1::4::9::16::25::[])
  return "test take"
  return 5
  return (2::5::8::11::14::17::20::23::26::29::32::35::38::41::44::47::50::53::56::59::62::[])
  return "test fold_left and fold_right"
  return 89
  return 161
  return "test forall, exists and mem"
  return false
  return true
  return false
  return "test filter"
  return (4::5::[])
  return "test complement and intersection"
  return (1::3::5::6::[])
  return (2::4::[])
  return "test zip and unzip"
  return ((1, "a")::(2, "b")::(3, "c")::[])
  return (1::2::3::[], "a"::"b"::"c"::[])
  return "test reverse"
  return (5::4::3::2::1::[])
  return "test concatenate lists"
  return (1::2::3::4::5::6::[])
  return "test length, hd and tl"
  return 5
  return 1
  return (2::3::4::[])
  return "test abs, min and max"
  return (5, 5, 5)
  return 1
  return 2
  return "test gcd and lcm"
  return 4
  return 24
  return "test odd and even"
  return false
  return true
  return "test id"
  return 5
  return id
  return "test compose and reverse apply"
  return 196
  return 7
  return "test fst and snd"
  return "foo"
  return 4
  ======================================================================
  tydef.mlt
  ======================================================================
  return Tail
  return (Node (10, Empty, Node (20, Empty, Empty)))
  ======================================================================
  type_annotations.mlt
  ======================================================================
  return (fun y ↦ return (fun z ↦ let b = let b = z y in b true in return b))
  ======================================================================
  typing.mlt
  ======================================================================
  return (fun y ↦ return y)
  return h
  ======================================================================
  use_undefined_type.mlt
  ======================================================================
  Syntax error (file "use_undefined_type.mlt", line 1, char 19):
  Unknown name --bar--
