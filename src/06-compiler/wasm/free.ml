open Utils.Source
open Types
module Set = Set.Make (Int)

type t = {
  types : Set.t;
  globals : Set.t;
  tables : Set.t;
  memories : Set.t;
  funcs : Set.t;
  elems : Set.t;
  datas : Set.t;
  locals : Set.t;
  labels : Set.t;
}

let empty : t =
  {
    types = Set.empty;
    globals = Set.empty;
    tables = Set.empty;
    memories = Set.empty;
    funcs = Set.empty;
    elems = Set.empty;
    datas = Set.empty;
    locals = Set.empty;
    labels = Set.empty;
  }

let union (s1 : t) (s2 : t) : t =
  {
    types = Set.union s1.types s2.types;
    globals = Set.union s1.globals s2.globals;
    tables = Set.union s1.tables s2.tables;
    memories = Set.union s1.memories s2.memories;
    funcs = Set.union s1.funcs s2.funcs;
    elems = Set.union s1.elems s2.elems;
    datas = Set.union s1.datas s2.datas;
    locals = Set.union s1.locals s2.locals;
    labels = Set.union s1.labels s2.labels;
  }

let types s = { empty with types = s }
let globals s = { empty with globals = s }
let tables s = { empty with tables = s }
let memories s = { empty with memories = s }
let funcs s = { empty with funcs = s }
let elems s = { empty with elems = s }
let datas s = { empty with datas = s }
let locals s = { empty with locals = s }
let labels s = { empty with labels = s }
let idx' x' = Set.singleton x'
let idx x = Set.singleton x.it
let zero = Set.singleton 0
let shift s = Set.map (Int.add (-1)) (Set.remove 0 s)
let map f = function Some x -> Some (f x) | None -> None
let get o x = match o with Some y -> y | None -> x
let ( ++ ) = union
let opt free xo = get (map free xo) empty
let list free xs = List.fold_left union empty (List.map free xs)
let var_type = function StatX x -> types (idx' x) | RecX _ -> empty
let num_type = function I32T | I64T | F32T | F64T -> empty
let vec_type = function V128T -> empty

let heap_type = function
  | AnyHT | NoneHT | EqHT | I31HT | StructHT | ArrayHT -> empty
  | FuncHT | NoFuncHT -> empty
  | ExternHT | NoExternHT -> empty
  | VarHT x -> var_type x
  | DefHT _ct -> empty (* assume closed *)
  | BotHT -> empty

let ref_type = function _, t -> heap_type t

let val_type = function
  | NumT t -> num_type t
  | VecT t -> vec_type t
  | RefT t -> ref_type t
  | BotT -> empty

let pack_type _ = empty

let storage_type = function
  | ValStorageT t -> val_type t
  | PackStorageT t -> pack_type t

let field_type (FieldT (_mut, st)) = storage_type st
let struct_type (StructT fts) = list field_type fts
let array_type (ArrayT ft) = field_type ft
let func_type (FuncT (ts1, ts2)) = list val_type ts1 ++ list val_type ts2

let str_type = function
  | DefStructT st -> struct_type st
  | DefArrayT at -> array_type at
  | DefFuncT ft -> func_type ft

let sub_type = function
  | SubT (_fin, hts, st) -> list heap_type hts ++ str_type st

let rec_type = function RecT sts -> list sub_type sts
let def_type = function DefT (rt, _i) -> rec_type rt
let global_type (GlobalT (_mut, t)) = val_type t
let table_type (TableT (_lim, t)) = ref_type t
let memory_type (MemoryT _lim) = empty
let type_ (t : Ast.type_) = rec_type t
