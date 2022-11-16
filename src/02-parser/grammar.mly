%{
  open SugaredAst
  open Utils
  open Utils.LongName
%}

%token LPAREN RPAREN LBRACK RBRACK
%token COLON COMMA SEMI EQUAL CONS
%token BEGIN END
%token <Utils.LongName.LongName.t> LNAME
%token <Utils.LongName.LongName.t> LONGLNAME
%token UNDERSCORE AS
%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token <float> FLOAT
%token <Utils.LongName.LongName.t> UNAME
%token <Utils.LongName.LongName.t> LONGUNAME
%token <SugaredAst.ty_param> PARAM
%token TYPE ARROW OF 
%token MODULE STRUCT
%token MATCH WITH FUNCTION
%token RUN LET REC AND IN
%token FUN BAR BARBAR
%token IF THEN ELSE
%token PLUS STAR MINUS MINUSDOT
%token LSL LSR ASR
%token MOD OR
%token AMPER AMPERAMPER
%token LAND LOR LXOR
%token <string> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4
%token EOF

%nonassoc ARROW IN
%right SEMI
%nonassoc ELSE
%right OR BARBAR
%right AMPER AMPERAMPER
%left  INFIXOP0 EQUAL
%right INFIXOP1
%right CONS
%left  INFIXOP2 PLUS MINUS MINUSDOT
%left  INFIXOP3 STAR MOD LAND LOR LXOR
%right INFIXOP4 LSL LSR ASR

%start <SugaredAst.term> payload
%start <SugaredAst.command list> commands

%%

(* Toplevel syntax *)

(* If you're going to "optimize" this, please make sure we don't require ;; at the
   end of the file. *)
commands:
  | EOF
     { [] }
  | cmd = command cmds = commands
     { cmd :: cmds }

(* Things that can be defined on toplevel. *)
command: mark_position(plain_command) { $1 }
plain_command:
  | TYPE defs = separated_nonempty_list(AND, ty_def)
    { TyDef defs }
  | MODULE name = UNAME EQUAL STRUCT defs = mod_defs(END)
    { Module (name, defs) }
  | LET x = ident t = lambdas0(EQUAL)
    { TopLet (x, t) }
  | LET REC def = let_rec_def
    { let (f, t) = def in TopLetRec (f, t) }
  | RUN trm = term
    { TopDo trm }

payload:
  | trm = term EOF
    { trm }

mod_defs(END):
  | END
     { [] }
  | def = mod_def defs = mod_defs(END)
     { def :: defs }

(* Things that can be defined in a module. *)
mod_def:
  | TYPE defs = separated_nonempty_list(AND, ty_def)
    { MTyDef defs }
  | LET x = ident t = lambdas0(EQUAL)
    { MTopLet (x, t) }
  | LET REC def = let_rec_def
    { let (f, t) = def in MTopLetRec (f, t) }
  | MODULE name = UNAME EQUAL STRUCT defs = mod_defs(END)
    { Module (name, defs) }

(* Main syntax tree *)

term: mark_position(plain_term) { $1 }
plain_term:
  | MATCH t = term WITH cases = cases0(case) (* END *)
    { Match (t, cases) }
  | FUNCTION cases = cases(case) (* END *)
    { Function cases }
  | FUN t = lambdas1(ARROW)
    { t.it }
  | LET def = let_def IN t2 = term
    { let (p, t1) = def in Let (p, t1, t2) }
  | LET REC def = let_rec_def IN t2 = term
    { let (f, t1) = def in LetRec (f, t1, t2) }
  | t1 = term SEMI t2 = term
    { Let ({it= PNonbinding; at= t1.at}, t1, t2) }
  | IF t_cond = comma_term THEN t_true = term ELSE t_false = term
    { Conditional (t_cond, t_true, t_false) }
  | t = plain_comma_term
    { t }

comma_term: mark_position(plain_comma_term) { $1 }
plain_comma_term:
  | t = binop_term COMMA ts = separated_list(COMMA, binop_term)
    { Tuple (t :: ts) }
  | t = plain_binop_term
    { t }

binop_term: mark_position(plain_binop_term) { $1 }
plain_binop_term:
  | t1 = binop_term op = binop t2 = binop_term
    { Apply ({it= Apply ({it= Var op; at=Location.of_lexeme $startpos}, t1); at=Location.of_lexeme $startpos}, t2) }
  | t1 = binop_term CONS t2 = binop_term
    { let tuple = {it= Tuple [t1; t2]; at= Location.of_lexeme $startpos} in
      Variant (LongName.from_id cons_label, Some tuple) }
  | t = plain_uminus_term
    { t }

uminus_term: mark_position(plain_uminus_term) { $1 }
plain_uminus_term:
  | MINUS t = uminus_term
    { let op_loc = Location.of_lexeme $startpos($1) in
      Apply ({it= Var (LongName.from_id "(~-)"); at= op_loc}, t) }
  | MINUSDOT t = uminus_term
    { let op_loc = Location.of_lexeme $startpos($1) in
      Apply ({it= Var (LongName.from_id "(~-.)"); at= op_loc}, t) }
  | t = plain_app_term
    { t }

plain_app_term:
  | t = prefix_term ts = prefix_term+
    {
      match t.it, ts with
      | Variant (lbl, None), [t] -> Variant (lbl, Some t)
      | Variant (lbl, _), _ -> Error.syntax ~loc:(t.at) "Label %s applied to too many argument" (LongName.to_string lbl)
      | _, _ ->
        let apply t1 t2 = {it= Apply(t1, t2); at= t1.at} in
        (List.fold_left apply t ts).it
    }
  | t = plain_prefix_term
    { t }

prefix_term: mark_position(plain_prefix_term) { $1 }
plain_prefix_term:
  | op = prefixop t = simple_term
    {
      let op_loc = Location.of_lexeme $startpos(op) in
      Apply ({it= Var (LongName.from_id op); at= op_loc}, t)
    }
  | t = plain_simple_term
    { t }

simple_term: mark_position(plain_simple_term) { $1 }
plain_simple_term:
  | x = refident
    { Var x }
  | lbl = longuname
    { Variant (lbl, None) }
  | cst = const
    { Const cst }
  | LBRACK ts = separated_list(SEMI, comma_term) RBRACK
    {
      let nil = {it= Variant (LongName.from_id nil_label, None); at= Location.of_lexeme $endpos} in
      let cons t ts =
        let loc = t.at in
        let tuple = {it= Tuple [t; ts];at= loc} in
        {it= Variant (LongName.from_id cons_label, Some tuple); at= loc}
      in
      (List.fold_right cons ts nil).it
    }
  | LPAREN RPAREN
    { Tuple [] }
  | LPAREN t = term COLON ty = ty RPAREN
    { Annotated (t, ty) }
  | LPAREN t = plain_term RPAREN
    { t }
  | BEGIN t = plain_term END
    { t }

(* Auxilliary definitions *)

const:
  | n = INT
    { Language.Const.of_integer n }
  | str = STRING
    { Language.Const.of_string str }
  | b = BOOL
    { Language.Const.of_boolean b }
  | f = FLOAT
    { Language.Const.of_float f }

case:
  | p = pattern ARROW t = term
    { (p, t) }

lambdas0(SEP):
  | SEP t = term
    { t }
  | p = simple_pattern t = lambdas0(SEP)
    { {it= Lambda (p, t); at= Location.of_lexeme $startpos} }
  | COLON ty = ty SEP t = term
    { {it= Annotated (t, ty); at= Location.of_lexeme $startpos} }

lambdas1(SEP):
  | p = simple_pattern t = lambdas0(SEP)
    { {it= Lambda (p, t); at= Location.of_lexeme $startpos} }

let_def:
  | p = pattern EQUAL t = term
    { (p, t) }
  | p = pattern COLON ty= ty EQUAL t = term
    { (p, {it= Annotated(t, ty); at= Location.of_lexeme $startpos}) }
  | x = mark_position(ident) t = lambdas1(EQUAL)
    { ({it= PVar x.it; at= x.at}, t) }

let_rec_def:
  | f = ident t = lambdas0(EQUAL)
    { (f, t) }

pattern: mark_position(plain_pattern) { $1 }
plain_pattern:
  | p = comma_pattern
    { p.it }
  | p = pattern AS x = lname
    { PAs (p, x) }

comma_pattern: mark_position(plain_comma_pattern) { $1 }
plain_comma_pattern:
  | ps = separated_nonempty_list(COMMA, cons_pattern)
    { match ps with [p] -> p.it | ps -> PTuple ps }

cons_pattern: mark_position(plain_cons_pattern) { $1 }
plain_cons_pattern:
  | p = variant_pattern
    { p.it }
  | p1 = variant_pattern CONS p2 = cons_pattern
    { let ptuple = {it= PTuple [p1; p2]; at= Location.of_lexeme $startpos} in
      PVariant (LongName.from_id cons_label, Some ptuple) }

variant_pattern: mark_position(plain_variant_pattern) { $1 }
plain_variant_pattern:
  | lbl = UNAME p = simple_pattern
    { PVariant (lbl, Some p) }
  | p = simple_pattern
    { p.it }

simple_pattern: mark_position(plain_simple_pattern) { $1 }
plain_simple_pattern:
  | x = ident
    { PVar x }
  | lbl = longuname
    { PVariant (lbl, None) }
  | UNDERSCORE
    { PNonbinding }
  | cst = const
    { PConst cst }
  | LBRACK ts = separated_list(SEMI, pattern) RBRACK
    {
      let nil = {it= PVariant (LongName.from_id nil_label, None);at= Location.of_lexeme $endpos} in
      let cons t ts =
        let loc = t.at in
        let tuple = {it= PTuple [t; ts]; at= loc} in
        {it= PVariant (LongName.from_id cons_label, Some tuple); at= loc}
      in
      (List.fold_right cons ts nil).it
    }
  | LPAREN RPAREN
    { PTuple [] }
  | LPAREN p = pattern COLON t = ty RPAREN
    { PAnnotated (p, t) }
  | LPAREN p = pattern RPAREN
    { p.it }

lname:
  | x = LNAME
    { x }

longlname:
  | x = lname
    { x }
  | p = LONGLNAME
    { p }

longuname:
  | x = UNAME
    { x }
  | p = LONGUNAME
    { p }

tyname:
  | t = lname
    { t }

ident:
  | x = lname
    { x }
  | LPAREN op = binop RPAREN
    { op }
  | LPAREN op = prefixop RPAREN
    { LongName.from_id op }

refident:
  | x = ident
    { x }
  | x = longlname
    { x }

%inline binop:
  | op = binop_symbol
    { LongName.from_id ("(" ^ op ^ ")") }

%inline binop_symbol:
  | OR
    { "or" }
  | BARBAR
    { "||" }
  | AMPER
    { "&" }
  | AMPERAMPER
    { "&&" }
  | op = INFIXOP0
    { op }
  | op = INFIXOP1
    { op }
  | op = INFIXOP2
    { op }
  | PLUS
    { "+" }
  | MINUSDOT
    { "-." }
  | MINUS
    { "-" }
  | EQUAL
    { "=" }
  | op = INFIXOP3
    { op }
  | STAR
    { "*" }
  | op = INFIXOP4
    { op }
  | MOD
    { "mod" }
  | LAND
    { "land" }
  | LOR
    { "lor" }
  | LXOR
    { "lxor" }
  | LSL
    { "lsl" }
  | LSR
    { "lsr" }
  | ASR
    { "asr" }

%inline prefixop:
  | op = PREFIXOP
    { "(" ^ op ^ ")" }

cases0(case):
  | BAR? cs = separated_list(BAR, case)
    { cs }

cases(case):
  | BAR? cs = separated_nonempty_list(BAR, case)
    { cs }

mark_position(X):
  x = X
  { {it= x; at= Location.of_lexeme $startpos}}

params:
  |
    { [] }
  | p = PARAM
    { [p] }
  | LPAREN ps = separated_nonempty_list(COMMA, PARAM) RPAREN
    { ps }

ty_def:
  | ps = params t = tyname EQUAL x = defined_ty
    { (ps, t, x) }

defined_ty:
  | variants = cases(sum_case)
    { TySum variants }
  | t = ty
    { TyInline t }

ty: mark_position(plain_ty) { $1 }
plain_ty:
  | t1 = ty_apply ARROW t2 = ty
    { TyArrow (t1, t2) }
  | t = plain_prod_ty
    { t }

plain_prod_ty:
  | ts = separated_nonempty_list(STAR, ty_apply)
    {
      match ts with
      | [] -> assert false
      | [t] -> t.it
      | _ -> TyTuple ts
     }

ty_apply: mark_position(plain_ty_apply) { $1 }
plain_ty_apply:
  | LPAREN t = ty COMMA ts = separated_nonempty_list(COMMA, ty) RPAREN t2 = tyname
    { TyApply (t2, (t :: ts)) }
  | t = ty_apply t2 = tyname
    { TyApply (t2, [t]) }
  | t = plain_simple_ty
    { t }

plain_simple_ty:
  | t = tyname
    { TyApply (t, []) }
  | t = PARAM
    { TyParam t }
  | LPAREN t = ty RPAREN
    { t.it }

sum_case:
  | lbl = UNAME
    { (lbl, None) }
  | lbl = UNAME OF t = ty
    { (lbl, Some t) }

%%
