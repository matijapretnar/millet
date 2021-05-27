  $ ../cli.exe test_stdlib.mlt
  val __compare_eq__ : α × α → bool
  val __compare_lt__ : α × α → bool
  val __compare_gt__ : α × α → bool
  val __compare_le__ : α × α → bool
  val __compare_ge__ : α × α → bool
  val __compare_ne__ : α × α → bool
  val __integer_add__ : int × int → int
  val __integer_mul__ : int × int → int
  val __integer_sub__ : int × int → int
  val __integer_div__ : int × int → int
  val __integer_mod__ : int × int → int
  val __integer_neg__ : int → int
  val __float_add__ : float × float → float
  val __float_mul__ : float × float → float
  val __float_sub__ : float × float → float
  val __float_div__ : float × float → float
  val __float_pow__ : float × float → float
  val __float_neg__ : float → float
  val to_string : α → string
  val (=) : α → α → bool
  val (<) : α → α → bool
  val (>) : α → α → bool
  val (<=) : α → α → bool
  val (>=) : α → α → bool
  val (<>) : α → α → bool
  val (+) : int → int → int
  val (*) : int → int → int
  val (-) : int → int → int
  val (/) : int → int → int
  val (mod) : int → int → int
  val (~-) : int → int
  val (+.) : float → float → float
  val (*.) : float → float → float
  val (-.) : float → float → float
  val (/.) : float → float → float
  val (**) : float → float → float
  val (~-.) : float → float
  val absurd : α → β
  val not : bool → bool
  type option
  val assoc : α → (α × β) list → β option
  val range : int → int → int list
  val reverse : α list → α list
  val map : (α → β) → α list → β list
  val hd : α list → α
  val tl : α list → α list
  val take : (int → α) → int → α list
  val fold_left : (α → β → α) → α → β list → α
  val fold_right : (α → β → β) → α list → β → β
  val iter : (α → β) → α list → unit
  val forall : (α → bool) → α list → bool
  val exists : (α → bool) → α list → bool
  val mem : α → α list → bool
  val filter : (α → bool) → α list → α list
  val complement : α list → α list → α list
  val intersection : α list → α list → α list
  val zip : α list → β list → (α × β) list
  val unzip : (α × β) list → α list × β list
  val (@) : α list → α list → α list
  val length : α list → int
  val nth : α list → int → α
  val abs : int → int
  val min : α → α → α
  val max : α → α → α
  val gcd : int → int → int
  val lcm : int → int → int
  val odd : int → bool
  val even : int → bool
  val id : α → α
  val compose : (α → β) → (γ → α) → γ → β
  val (|>) : α → (α → β) → β
  val ignore : α → unit
  val fst : α × β → α
  val snd : α × β → β
  val return : α → α
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
  return 9
  return (6::9::12::15::18::21::24::27::30::33::36::39::42::45::48::51::54::57::60::63::66::[])
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
