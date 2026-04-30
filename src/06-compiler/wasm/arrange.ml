open Pack
open Ast
open Types
open Sexpr
module Free = Free

(* Generic formatting *)

let nat n = string_of_int n
let nat32 = string_of_int
let add_hex_char buf c = Printf.bprintf buf "\\%02x" (Char.code c)

let add_char buf = function
  | '\n' -> Buffer.add_string buf "\\n"
  | '\t' -> Buffer.add_string buf "\\t"
  | '\"' -> Buffer.add_string buf "\\\""
  | '\\' -> Buffer.add_string buf "\\\\"
  | c when '\x20' <= c && c < '\x7f' -> Buffer.add_char buf c
  | c -> add_hex_char buf c

let add_unicode_char buf = function
  | (0x09 | 0x0a) as uc -> add_char buf (Char.chr uc)
  | uc when 0x20 <= uc && uc < 0x7f -> add_char buf (Char.chr uc)
  | uc -> Printf.bprintf buf "\\u{%02x}" uc

let string_with iter add_char s =
  let buf = Buffer.create 256 in
  Buffer.add_char buf '\"';
  iter (add_char buf) s;
  Buffer.add_char buf '\"';
  Buffer.contents buf

let bytes = string_with String.iter add_hex_char
let string = string_with String.iter add_char
let name = string_with List.iter add_unicode_char
let list_of_opt = function None -> [] | Some x -> [ x ]
let list f xs = List.map f xs
let listi f xs = List.mapi f xs
let opt f xo = list f (list_of_opt xo)
let opt_s f xo = Free.get (Free.map f xo) ""
let tab head f xs = if xs = [] then [] else [ Node (head, list f xs) ]
let atom f x = Atom (f x)

let breakup s n =
  let rec loop i =
    let len = min n (String.length s - i) in
    if len = 0 then [] else String.sub s i len :: loop (i + len)
  in
  loop 0

let break_bytes s =
  let ss = breakup s 16 in
  list (atom bytes) ss

let split s c =
  let len = String.length s in
  let rec loop i =
    if i > len then []
    else
      let j = try String.index_from s i c with Not_found -> len in
      String.sub s i (j - i) :: loop (j + 1)
  in
  loop 0

let rec split_last = function
  | x :: [] -> ([], x)
  | x :: xs ->
      let ys, y = split_last xs in
      (x :: ys, y)
  | [] -> failwith "split_last"

let break_string s =
  let ss, s' = split_last (split s '\n') in
  list (atom string) (List.map (fun s -> s ^ "\n") ss @ [ s' ])

(* Types *)

let mutability node = function Cons -> node | Var -> Node ("mut", [ node ])
let num_type t = string_of_num_type t
let vec_type t = string_of_vec_type t

let ref_type t =
  match t with
  | Null, AnyHT -> "anyref"
  | Null, EqHT -> "eqref"
  | Null, I31HT -> "i31ref"
  | Null, StructHT -> "structref"
  | Null, ArrayHT -> "arrayref"
  | Null, FuncHT -> "funcref"
  | t -> string_of_ref_type t

let heap_type t = string_of_heap_type t
let val_type t = string_of_val_type t
let storage_type t = string_of_storage_type t
let final = function NoFinal -> "" | Final -> " final"
let decls kind ts = tab kind (atom val_type) ts
let field_type (FieldT (mut, t)) = mutability (atom storage_type t) mut

let struct_type (StructT fts) =
  Node ("struct", list (fun ft -> Node ("field", [ field_type ft ])) fts)

let array_type (ArrayT ft) = Node ("array", [ field_type ft ])

let func_type (FuncT (ts1, ts2)) =
  Node ("func", decls "param" ts1 @ decls "result" ts2)

let str_type st =
  match st with
  | DefStructT st -> struct_type st
  | DefArrayT at -> array_type at
  | DefFuncT ft -> func_type ft

let sub_type = function
  | SubT (Final, [], st) -> str_type st
  | SubT (fin, xs, st) ->
      Node
        ( String.concat " " (("sub" ^ final fin) :: List.map heap_type xs),
          [ str_type st ] )

let rec_type i j st = Node ("type $" ^ nat (i + j), [ sub_type st ])
let limits nat { min; max } = String.concat " " (nat min :: opt nat max)
let global_type (GlobalT (mut, t)) = mutability (atom string_of_val_type t) mut

let pack_size = function
  | Pack8 -> "8"
  | Pack16 -> "16"
  | Pack32 -> "32"
  | Pack64 -> "64"

let extension = function SX -> "_s" | ZX -> "_u"

let pack_shape = function
  | Pack8x8 -> "8x8"
  | Pack16x4 -> "16x4"
  | Pack32x2 -> "32x2"

let vec_extension sz = function
  | ExtLane (sh, ext) -> pack_shape sh ^ extension ext
  | ExtSplat -> pack_size sz ^ "_splat"
  | ExtZero -> pack_size sz ^ "_zero"

(* Operators *)

let memop name typ { ty; align; offset; _ } sz =
  typ ty ^ "." ^ name
  ^ (if offset = 0 then "" else " offset=" ^ nat32 offset)
  ^ if 1 lsl align = sz then "" else " align=" ^ nat (1 lsl align)

let loadop op =
  match op.pack with
  | None -> memop "load" num_type op (num_size op.ty)
  | Some (sz, ext) ->
      memop ("load" ^ pack_size sz ^ extension ext) num_type op (packed_size sz)

let storeop op =
  match op.pack with
  | None -> memop "store" num_type op (num_size op.ty)
  | Some sz -> memop ("store" ^ pack_size sz) num_type op (packed_size sz)

let vec_loadop op =
  match op.pack with
  | None -> memop "load" vec_type op (vec_size op.ty)
  | Some (sz, ext) ->
      memop ("load" ^ vec_extension sz ext) vec_type op (packed_size sz)

let vec_storeop op = memop "store" vec_type op (vec_size op.ty)

let vec_laneop instr (op, i) =
  memop (instr ^ pack_size op.pack ^ "_lane") vec_type op (packed_size op.pack)
  ^ " " ^ nat i

let initop = function Explicit -> "" | Implicit -> "_default"

(* Expressions *)

let var x = nat32 x

let block_type = function
  | VarBlockType x -> [ Node ("type " ^ var x, []) ]
  | ValBlockType ts -> decls "result" (list_of_opt ts)

let suffix = function I32T | I64T -> "_s" | F32T | F64T -> ""

let rec instr e =
  let head, inner =
    match e with
    | Unreachable -> ("unreachable", [])
    | Drop -> ("drop", [])
    | Block (bt, es) -> ("block", block_type bt @ list instr es)
    | Loop (bt, es) -> ("loop", block_type bt @ list instr es)
    | If (bt, es1, es2) ->
        ( "if",
          block_type bt
          @ [ Node ("then", list instr es1); Node ("else", list instr es2) ] )
    | Br x -> ("br " ^ string_of_int x, [])
    | BrIf x -> ("br_if " ^ string_of_int x, [])
    | BrTable (xs, x) ->
        ( "br_table " ^ String.concat " " (List.map string_of_int (xs @ [ x ])),
          [] )
    | BrOnCast (x, t1, t2) ->
        ("br_on_cast " ^ var x, [ Atom (ref_type t1); Atom (ref_type t2) ])
    | Return -> ("return", [])
    | Call x -> ("call " ^ string_of_int x, [])
    | CallRef x -> ("call_ref " ^ string_of_int x, [])
    | LocalGet x -> ("local.get " ^ string_of_int x, [])
    | LocalSet x -> ("local.set " ^ string_of_int x, [])
    | LocalTee x -> ("local.tee " ^ string_of_int x, [])
    | GlobalGet x -> ("global.get " ^ string_of_int x, [])
    | GlobalSet x -> ("global.set " ^ string_of_int x, [])
    | Load op -> (loadop op, [])
    | MemorySize -> ("memory.size", [])
    | MemoryGrow -> ("memory.grow", [])
    | MemoryInit x -> ("memory.init " ^ string_of_int x, [])
    | RefNull t -> ("ref.null", [ Atom (heap_type t) ])
    | RefFunc x -> ("ref.func " ^ string_of_int x, [])
    | RefIsNull -> ("ref.is_null", [])
    | RefAsNonNull -> ("ref.as_non_null", [])
    | RefCast t -> ("ref.cast", [ Atom (ref_type t) ])
    | RefEq -> ("ref.eq", [])
    | RefI31 -> ("ref.i31", [])
    | I31Get ext -> ("i31.get" ^ extension ext, [])
    | StructNew (x, op) -> ("struct.new" ^ initop op ^ " " ^ string_of_int x, [])
    | StructGet (x, y, exto) ->
        ( "struct.get" ^ opt_s extension exto ^ " " ^ string_of_int x ^ " "
          ^ string_of_int y,
          [] )
    | StructSet (x, y) ->
        ("struct.set " ^ string_of_int x ^ " " ^ string_of_int y, [])
    | ArrayNew (x, op) -> ("array.new" ^ initop op ^ " " ^ string_of_int x, [])
    | ArrayGet (x, exto) ->
        ("array.get" ^ opt_s extension exto ^ " " ^ string_of_int x, [])
    | ArraySet x -> ("array.set " ^ string_of_int x, [])
    | ArrayLen -> ("array.len", [])
    | Add nt -> (num_type nt ^ ".add", [])
    | Sub nt -> (num_type nt ^ ".sub", [])
    | Mul nt -> (num_type nt ^ ".mul", [])
    | Div nt -> (num_type nt ^ ".div", [])
    | DivS nt -> (num_type nt ^ ".div_s", [])
    | Shl nt -> (num_type nt ^ ".shl", [])
    | Shr nt -> (num_type nt ^ ".shr", [])
    | ShrU nt -> (num_type nt ^ ".shr_u", [])
    | Ne nt -> (num_type nt ^ ".ne", [])
    | Rem nt -> (num_type nt ^ ".rem_s", [])
    | Eq nt -> (num_type nt ^ ".eq", [])
    | Eqz nt -> (num_type nt ^ ".eqz", [])
    | Neg nt -> (num_type nt ^ ".neg", [])
    | Le nt -> (num_type nt ^ ".le" ^ suffix nt, [])
    | Lt nt -> (num_type nt ^ ".lt" ^ suffix nt, [])
    | Ge nt -> (num_type nt ^ ".ge" ^ suffix nt, [])
    | Gt nt -> (num_type nt ^ ".gt" ^ suffix nt, [])
    | IntConst (nt, i) -> (num_type nt ^ ".const " ^ string_of_int i, [])
    | FloatConst (nt, f) -> (num_type nt ^ ".const " ^ string_of_float f, [])
  in
  Node (head, inner)

let const head c =
  match c with [ e ] -> instr e | _ -> Node (head, list instr c)

(* Functions *)

let func_with_name name f =
  let { ftype; locals; body } = f in
  Node
    ( "func" ^ name,
      [ Node ("type " ^ var ftype, []) ]
      @ decls "local" (List.map (fun loc -> loc.ltype) locals)
      @ list instr body )

let func_with_index off i f = func_with_name (" $" ^ nat (off + i)) f
let func f = func_with_name "" f

(* Tables & memories *)

let table off i tab =
  let { ttype = TableT (lim, t); tinit } = tab in
  Node
    ( "table $" ^ nat (off + i) ^ " " ^ limits nat32 lim,
      atom ref_type t :: list instr tinit )

let memory off i mem =
  let { mtype = MemoryT lim } = mem in
  Node ("memory $" ^ nat (off + i) ^ " " ^ limits nat32 lim, [])

let is_elem_kind = function NoNull, FuncHT -> true | _ -> false
let elem_kind = function NoNull, FuncHT -> "func" | _ -> assert false
let is_elem_index e = match e with [ RefFunc _ ] -> true | _ -> false

let elem_index e =
  match e with [ RefFunc x ] -> atom var x | _ -> assert false

let segment_mode category mode =
  match mode with
  | Passive -> []
  | Active { index; offset } ->
      (if index = 0 then [] else [ Node (category, [ atom var index ]) ])
      @ [ const "offset" offset ]
  | Declarative -> [ Atom "declare" ]

let elem i seg =
  let { etype; einit; emode } = seg in
  Node
    ( "elem $" ^ nat i,
      segment_mode "table" emode
      @
      if is_elem_kind etype && List.for_all is_elem_index einit then
        atom elem_kind etype :: list elem_index einit
      else atom ref_type etype :: list (const "item") einit )

let data i seg =
  let { dinit; dmode } = seg in
  Node ("data $" ^ nat i, segment_mode "memory" dmode @ break_bytes dinit)

(* Modules *)

let type_ (ns, i) (ty : rec_type) =
  match ty with
  | RecT [ st ] when not Free.(Set.mem i (type_ ty).types) ->
      (rec_type i 0 st :: ns, i + 1)
  | RecT sts ->
      (Node ("rec", List.mapi (rec_type i) sts) :: ns, i + List.length sts)

let import_desc fx tx mx gx d =
  match d with
  | FuncImport x ->
      incr fx;
      Node ("func $" ^ nat (!fx - 1), [ Node ("type", [ atom var x ]) ])
  | TableImport t ->
      incr tx;
      table 0 (!tx - 1) { ttype = t; tinit = [] }
  | MemoryImport t ->
      incr mx;
      memory 0 (!mx - 1) { mtype = t }
  | GlobalImport t ->
      incr gx;
      Node ("global $" ^ nat (!gx - 1), [ global_type t ])

let import fx tx mx gx im =
  let { module_name; item_name; idesc } = im in
  Node
    ( "import",
      [
        atom name module_name;
        atom name item_name;
        import_desc fx tx mx gx idesc;
      ] )

let export_desc d =
  match d with
  | FuncExport x -> Node ("func", [ atom var x ])
  | TableExport x -> Node ("table", [ atom var x ])
  | MemoryExport x -> Node ("memory", [ atom var x ])
  | GlobalExport x -> Node ("global", [ atom var x ])

let export ex =
  let { name = n; edesc } = ex in
  Node ("export", [ atom name n; export_desc edesc ])

let global off i g =
  let { gtype; ginit } = g in
  Node ("global $" ^ nat (off + i), global_type gtype :: list instr ginit)

let start s = Node ("start " ^ var s.sfunc, [])

(* Modules *)

let var_opt = function None -> "" | Some x -> " " ^ x

let module_with_var_opt x_opt m =
  let fx = ref 0 in
  let tx = ref 0 in
  let mx = ref 0 in
  let gx = ref 0 in
  let imports = list (import fx tx mx gx) m.imports in
  Node
    ( "module" ^ var_opt x_opt,
      List.rev (fst (List.fold_left type_ ([], 0) m.types))
      @ imports
      @ listi (table !tx) m.tables
      @ listi (memory !mx) m.memories
      @ listi (global !gx) m.globals
      @ listi (func_with_index !fx) m.funcs
      @ list export m.exports @ opt start m.start @ listi elem m.elems
      @ listi data m.datas )

let binary_module_with_var_opt x_opt bs =
  Node ("module" ^ var_opt x_opt ^ " binary", break_bytes bs)

let quoted_module_with_var_opt x_opt s =
  Node ("module" ^ var_opt x_opt ^ " quote", break_string s)

let module_ = module_with_var_opt None
