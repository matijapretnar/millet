open Utils
open Utils.LongName

(* TODO: SugaredAst needs to be cleaned up. Not all ty_names, labels, etc. are LongNames.*)

type ty_name = LongName.t

let bool_ty_name = "bool"
let int_ty_name = "int"
let unit_ty_name = "unit"
let string_ty_name = "string"
let float_ty_name = "float"
let list_ty_name = "list"
let empty_ty_name = "empty"

type 'a annotated = { it : 'a; at : Location.t }
type ty_param = LongName.t

type ty = plain_ty annotated

and plain_ty =
  | TyConst of Language.Const.ty
  | TyApply of ty_name * ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of ty * ty  (** [ty1 -> ty2] *)
  | TyTuple of ty list  (** [ty1 * ty2 * ... * tyn] *)

type mod_name = LongName.t
type variable = LongName.t
type label = LongName.t
type operation = LongName.t

let nil_label = Language.Ast.nil_label_string
let cons_label = Language.Ast.cons_label_string

type pattern = plain_pattern annotated

and plain_pattern =
  | PVar of variable
  | PAnnotated of pattern * ty
  | PAs of pattern * variable
  | PTuple of pattern list
  | PVariant of label * pattern option
  | PConst of Language.Const.t
  | PNonbinding

type term = plain_term annotated

and plain_term =
  | Var of variable  (** variables *)
  | Const of Language.Const.t  (** integers, strings, booleans, and floats *)
  | Annotated of term * ty
  | Tuple of term list  (** [(t1, t2, ..., tn)] *)
  | Variant of label * term option  (** [Label] or [Label t] *)
  | Lambda of abstraction  (** [fun p1 p2 ... pn -> t] *)
  | Function of abstraction list  (** [function p1 -> t1 | ... | pn -> tn] *)
  | Let of pattern * term * term  (** [let p = t1 in t2] *)
  | LetRec of variable * term * term  (** [let rec f = t1 in t2] *)
  | Match of term * abstraction list
      (** [match t with p1 -> t1 | ... | pn -> tn] *)
  | Conditional of term * term * term  (** [if t then t1 else t2] *)
  | Apply of term * term  (** [t1 t2] *)

and abstraction = pattern * term
and guarded_abstraction = pattern * term option * term

type ty_def =
  | TySum of (label * ty option) list
      (** [Label1 of ty1 | Label2 of ty2 | ... | Labeln of tyn | Label' | Label''] *)
  | TyInline of ty  (** [ty] *)

type mod_def =
  | MTyDef of (ty_param list * ty_name * ty_def) list
  | MTopLet of variable * term  (** [let x = t] *)
  | MTopLetRec of variable * term  (** [let rec f = t] *)
  | Module of mod_name * mod_def list  (** [module M = struct ... end] *)

type command = plain_command annotated

and plain_command =
  | TyDef of (ty_param list * ty_name * ty_def) list
  | Module of mod_name * mod_def list  (** [module M = struct ... end] *)
  | TopLet of variable * term  (** [let x = t] *)
  | TopLetRec of variable * term  (** [let rec f = t] *)
  | TopDo of term  (** [do t] *)
