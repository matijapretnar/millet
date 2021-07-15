  $ for f in *.mlt
  > do
  >   echo "======================================================================"
  >   echo $f
  >   echo "======================================================================"
  >   ../cli.exe $f
  >   echo ""
  > done
  ======================================================================
  duplicate_variant_tydef_sum.mlt
  ======================================================================
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
  type cow
  
  ======================================================================
  invalid_match_type.mlt
  ======================================================================
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
  type a
  type b
  Typing error: Cannot unify a list = b
  
  ======================================================================
  less_than_function.mlt
  ======================================================================
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
  return true
  ======================================================================
  lexer.mlt
  ======================================================================
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
  type foo
  type bar
  
  ======================================================================
  nat.mlt
  ======================================================================
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
  type nat
  val add : nat → nat → nat
  val multiply : nat → nat → nat
  val to_int : nat → int
  val from_int : int → nat
  return 42
  ======================================================================
  non_linear_pattern.mlt
  ======================================================================
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
  return 20
  ======================================================================
  occurs_check.mlt
  ======================================================================
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
  Typing error: Cannot unify α = β → α
  
  ======================================================================
  orelse_andalso.mlt
  ======================================================================
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
  
  ======================================================================
  patterns.mlt
  ======================================================================
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
  ======================================================================
  polymorphism.mlt
  ======================================================================
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
  val f : α → α
  val g : α → β → α
  val u : α list
  val v : α list list
  val u : α → β
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
  val u : α → α
  Typing error: Cannot unify int = string
  
  ======================================================================
  recursion.mlt
  ======================================================================
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
  val fact : int → int
  val fib : int → int
  val gcd : int → int → int
  return 5
  ======================================================================
  shadow_label.mlt
  ======================================================================
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
  type cow
  type bull
  
  ======================================================================
  shadow_type.mlt
  ======================================================================
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
  type cow
  type bull
  type cow
  
  ======================================================================
  test_equality.mlt
  ======================================================================
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
  val f : α → α → bool
  return true
  return false
  return true
  return false
  return false
  return true
  ======================================================================
  test_less_then.mlt
  ======================================================================
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
  test_stdlib.mlt
  ======================================================================
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
  ======================================================================
  tydef.mlt
  ======================================================================
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
  type baire
  type tree
  type assoc
  type cow
  type bull
  return Tail
  return (Node (10, Empty, Node (20, Empty, Empty)))
  ======================================================================
  type_annotations.mlt
  ======================================================================
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
  val f : int → int
  val f : int → int
  val f : bool → int → (int → bool → int) → int
  val f : int → int × int
  val g : α → α
  val g : int → int
  val g : α → α → α
  val g : α → α × α
  val f : int → int
  type cow
  val f : (int, α) cow → int
  val f : (α, int) cow → int
  val x : int list
  return (fun y ↦ return (fun z ↦ let b = let b = z y in b true in return b))
  ======================================================================
  typing.mlt
  ======================================================================
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
  val b : int
  val b : bool
  val b : string
  val b : unit
  val b : float
  val t : int × int
  val t : α list × string
  type cow
  val v : (string, α) cow
  val v : (α, string) cow
  val v : (int, string list) cow → (string list, int) cow
  val f : α → α
  val f : α → α × α
  val f : (α → α) × β list
  val f : α → β → α
  val v : α list list list
  val f : α → β → α
  val h : α → α
  return (fun y ↦ return y)
  return h
  ======================================================================
  use_undefined_type.mlt
  ======================================================================
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
  Syntax error (file "use_undefined_type.mlt", line 1, char 19):
  Unknown name --bar--
  
