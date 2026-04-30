module Types = Types
module Source = Utils.Source
module Pack = Pack

(*pack*)
type initop = Explicit | Implicit
type ('t, 'p) memop = { ty : 't; align : int; offset : int; pack : 'p }
type loadop = (Types.num_type, (Pack.pack_size * Pack.extension) option) memop

(* Instructions *)
type instr =
  (* Constants *)
  | IntConst of Types.num_type * int
  | FloatConst of Types.num_type * float
  | LocalGet of int
  | LocalSet of int
  | LocalTee of int
  (* Arithmetic operations *)
  | Add of Types.num_type
  | Sub of Types.num_type
  | Mul of Types.num_type
  | Div of Types.num_type
  | Shl of Types.num_type
  | Shr of Types.num_type
  | ShrU of Types.num_type
  | DivS of Types.num_type
  | Rem of Types.num_type
  | Neg of Types.num_type
  | Ne of Types.num_type
  | Eq of Types.num_type
  | Eqz of Types.num_type
  | Lt of Types.num_type
  | Le of Types.num_type
  | Ge of Types.num_type
  | Gt of Types.num_type
  (* Control flow *)
  | Call of int
  | CallRef of int
  | Return
  | If of
      Types.block_type
      * instr list
      * instr list (* (if (then ... ) (else ... )) *)
  | Block of Types.block_type * instr list (* (block ... ) *)
  | Loop of Types.block_type * instr list (* (loop ... ) *)
  | Br of int (* (br <label_index>) *)
  | BrIf of int (* (br_if <label_index>) *)
  | BrOnCast of int * Types.ref_type * Types.ref_type (* break on type *)
  | BrTable of int list * int (* (br_table <labels> <default>) *)
  | Unreachable (* (unreachable) *)
  (* Struct operations *)
  | StructNew of int * initop
  | StructGet of int * int * Pack.extension option
  | StructSet of int * int
  (* globals *)
  | GlobalGet of int
  | GlobalSet of int
  (* Parametric instucrions *)
  | Drop
  (* Array operations *)
  | ArrayNew of int * initop
  | ArrayGet of int * Pack.extension option
  | ArraySet of int
  | ArrayLen (* Reference types operations *)
  | RefNull of Types.heap_type
  | RefAsNonNull
  | RefEq
  | RefIsNull
  | RefFunc of int
  | RefI31
  | I31Get of Pack.extension
  | RefCast of Types.ref_type
  (* memory *)
  | MemoryInit of int
  | MemoryGrow
  | MemorySize
  | Load of loadop

(*local*)
type const = instr list

type local = { ltype : Types.val_type }
and global = { gtype : Types.global_type; ginit : const }

(* Function *)
type func = { ftype : int; locals : local list; body : instr list }

(* Tables & Memories *)

type table = { ttype : Types.table_type; tinit : const }
type memory = { mtype : Types.memory_type }

type segment_mode =
  | Passive
  | Active of { index : int; offset : const }
  | Declarative

type elem_segment = {
  etype : Types.ref_type;
  einit : const list;
  emode : segment_mode;
}

type data_segment = { dinit : string; dmode : segment_mode }

(* Modules *)

type type_ = Types.rec_type

type export_desc =
  | FuncExport of int
  | TableExport of int
  | MemoryExport of int
  | GlobalExport of int

type export = { name : Types.name; edesc : export_desc }

type import_desc =
  | FuncImport of int
  | TableImport of Types.table_type
  | MemoryImport of Types.memory_type
  | GlobalImport of Types.global_type

type import = {
  module_name : Types.name;
  item_name : Types.name;
  idesc : import_desc;
}

type start = { sfunc : int }

type module_ = {
  types : type_ list;
  globals : global list;
  tables : table list;
  memories : memory list;
  funcs : func list;
  start : start option;
  elems : elem_segment list;
  datas : data_segment list;
  imports : import list;
  exports : export list;
}

(* Auxiliary functions *)

let empty_module =
  {
    types = [];
    globals = [];
    tables = [];
    memories = [];
    funcs = [];
    start = None;
    elems = [];
    datas = [];
    imports = [];
    exports = [];
  }
