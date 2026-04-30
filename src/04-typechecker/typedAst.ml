module Ast = Language.Ast
open Utils
module Const = Language.Const

let make_phrase x t = Source.{ it = x; et = Some t }

type pattern = (pattern', Ast.ty) Source.phrase

and pattern' =
  | PVar of Ast.variable
  | PAnnotated of pattern * Ast.ty
  | PAs of pattern * Ast.variable
  | PTuple of pattern list
  | PVariant of Ast.label * pattern option
  | PConst of Const.t
  | PNonbinding

type expression = (expression', Ast.ty) Source.phrase

and expression' =
  | Var of Ast.variable
  | Const of Const.t
  | Annotated of expression * Ast.ty
  | Tuple of expression list
  | Variant of Ast.label * expression option
  | Lambda of abstraction
  | RecLambda of Ast.variable * abstraction

and computation = (computation', Ast.ty) Source.phrase

and computation' =
  | Return of expression
  | Do of computation * abstraction
  | Match of expression * abstraction list
  | Apply of expression * expression

and abstraction = (abstraction', Ast.ty) Source.phrase
and abstraction' = pattern * computation

and command =
  | TyDef of (Ast.ty_param list * Ast.ty_name * Ast.ty_def) list
  | TopLet of Ast.variable * expression
  | TopDo of computation

let rec print_pattern ?max_level p ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match Source.it p with
  | PVar x -> print "%t" (Ast.Variable.print x)
  | PAs (p, x) -> print "%t as %t" (print_pattern p) (Ast.Variable.print x)
  | PAnnotated (p, _ty) -> print_pattern ?max_level p ppf
  | PConst c -> Const.print c ppf
  | PTuple lst -> Print.print_tuple print_pattern lst ppf
  | PVariant (lbl, None) when lbl = Ast.nil_label -> print "[]"
  | PVariant (lbl, None) -> print "%t" (Ast.Label.print lbl)
  | PVariant (lbl, Some { it = PTuple [ v1; v2 ]; _ }) when lbl = Ast.cons_label
    ->
      print "%t::%t" (print_pattern v1) (print_pattern v2)
  | PVariant (lbl, Some p) ->
      print ~at_level:1 "%t @[<hov>%t@]" (Ast.Label.print lbl) (print_pattern p)
  | PNonbinding -> print "_"

let rec print_expression ?max_level e ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match Source.it e with
  | Var x -> print "%t" (Ast.Variable.print x)
  | Const c -> print "%t" (Const.print c)
  | Annotated (t, _ty) -> print_expression ?max_level t ppf
  | Tuple lst -> Print.print_tuple print_expression lst ppf
  | Variant (lbl, None) when lbl = Ast.nil_label -> print "[]"
  | Variant (lbl, None) -> print "%t" (Ast.Label.print lbl)
  | Variant (lbl, Some { it = Tuple [ v1; v2 ]; _ }) when lbl = Ast.cons_label
    ->
      print ~at_level:1 "%t::%t"
        (print_expression ~max_level:0 v1)
        (print_expression ~max_level:1 v2)
  | Variant (lbl, Some e) ->
      print ~at_level:1 "%t @[<hov>%t@]" (Ast.Label.print lbl)
        (print_expression ~max_level:0 e)
  | Lambda a -> print ~at_level:2 "fun %t" (print_abstraction a)
  | RecLambda (f, _ty) -> print ~at_level:2 "rec %t ..." (Ast.Variable.print f)

and print_computation ?max_level c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match Source.it c with
  | Return e -> print ~at_level:1 "return %t" (print_expression ~max_level:0 e)
  | Do (c1, { it = { it = PNonbinding; _ }, c2; _ }) ->
      print "@[<hov>%t;@ %t@]" (print_computation c1) (print_computation c2)
  | Do (c1, { it = pat, c2; _ }) ->
      print "@[<hov>let@[<hov>@ %t =@ %t@] in@ %t@]" (print_pattern pat)
        (print_computation c1) (print_computation c2)
  | Match (e, lst) ->
      print "match %t with (@[<hov>%t@])" (print_expression e)
        (Print.print_sequence " | " print_case lst)
  | Apply (e1, e2) ->
      print ~at_level:1 "@[%t@ %t@]"
        (print_expression ~max_level:1 e1)
        (print_expression ~max_level:0 e2)

and print_abstraction { it = p, c; _ } ppf =
  Format.fprintf ppf "%t ↦ %t" (print_pattern p) (print_computation c)

and print_case a ppf = Format.fprintf ppf "%t" (print_abstraction a)

let string_of_expression e =
  print_expression e Format.str_formatter;
  Format.flush_str_formatter ()

let string_of_computation c =
  print_computation c Format.str_formatter;
  Format.flush_str_formatter ()

let make_pattern pat ty : pattern = make_phrase pat ty
let make_expression exp ty : expression = make_phrase exp ty
let make_computation comp ty : computation = make_phrase comp ty
let make_abstraction abs ty : abstraction = make_phrase abs ty

let make_const_expr const : expression =
  match const with
  | Ast.Const (Const.String s) ->
      Source.
        { it = Const (Const.String s); et = Some (Ast.TyConst Const.StringTy) }
  | Ast.Const (Const.Float f) ->
      Source.
        { it = Const (Const.Float f); et = Some (Ast.TyConst Const.FloatTy) }
  | Ast.Const (Const.Integer i) ->
      Source.
        {
          it = Const (Const.Integer i);
          et = Some (Ast.TyConst Const.IntegerTy);
        }
  | Ast.Const (Const.Boolean b) ->
      Source.
        {
          it = Const (Const.Boolean b);
          et = Some (Ast.TyConst Const.BooleanTy);
        }
  | _ -> failwith "this function only takes constant expressions"

let make_const_pat const : pattern =
  match const with
  | Ast.PConst (Const.String s) ->
      Source.
        { it = PConst (Const.String s); et = Some (Ast.TyConst Const.StringTy) }
  | Ast.PConst (Const.Float f) ->
      Source.
        { it = PConst (Const.Float f); et = Some (Ast.TyConst Const.FloatTy) }
  | Ast.PConst (Const.Integer i) ->
      Source.
        {
          it = PConst (Const.Integer i);
          et = Some (Ast.TyConst Const.IntegerTy);
        }
  | Ast.PConst (Const.Boolean b) ->
      Source.
        {
          it = PConst (Const.Boolean b);
          et = Some (Ast.TyConst Const.BooleanTy);
        }
  | _ -> failwith "this function only takes constant patterns"

let ( ++ ) s1 s2 = Ast.VariableMap.union (fun _ _ y2 -> Some y2) s1 s2

let ( -- ) s1 s2 =
  Ast.VariableMap.fold (fun x _ m1' -> Ast.VariableMap.remove x m1') s2 s1

let empty = Ast.VariableMap.empty
let list f xs = List.fold_left ( ++ ) empty (List.map f xs)
let val_var x t = Ast.VariableMap.singleton x t

let rec bound_pat p =
  let t = Source.et p in
  match Source.it p with
  | PVar x -> val_var x t
  | PAnnotated (pat, _) -> bound_pat pat
  | PAs (_, var) -> val_var var t
  | PTuple ps -> list bound_pat ps
  | PVariant (_, pat) -> (
      match pat with Some x -> bound_pat x | None -> empty)
  | PConst _ | PNonbinding -> empty

let rec free_exp e =
  let ty = Source.et e in
  match Source.it e with
  | Var var -> val_var var ty
  | Const _ -> empty
  | Annotated (exp, _) -> free_exp exp
  | Tuple exps -> list free_exp exps
  | Variant (_, exp) -> ( match exp with Some x -> free_exp x | None -> empty)
  | Lambda abs -> free_case abs
  | RecLambda (var, abs) -> free_case abs -- val_var var ty

and free_case { it = p, c; _ } =
  let free_c = free_comp c in
  let bound_p = bound_pat p in
  free_c -- bound_p

and free_comp c =
  match Source.it c with
  | Return exp -> free_exp exp
  | Do (comp, abs) -> free_comp comp ++ free_case abs
  | Match (exp, pes) -> free_exp exp ++ list free_case pes
  | Apply (e1, e2) -> free_exp e1 ++ free_exp e2
