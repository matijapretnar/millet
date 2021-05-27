  $ ../cli.exe patterns.mlt
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
  return 5
  return (1, 2)
  return (1, 2::3::4::[])
  return (2::3::4::[])
