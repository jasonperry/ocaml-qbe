
(* I don't need to model constants themselves, because they will be
   specified in the codegen directly? *)

exception BadQBE of string

(* type qbeIntegral =
  | Word
  | Long
  | Byte
  | Halfword

(* could I use polymorphic variants to include these under a
   single umbrella? *)

type qbeFloating =
  | Single
  | Double

type qbePrimitive =
  | Integral of qbeIntegral
  | Floating of qbeFloating

type qbetype =
  | Primitive of qbePrimitive
  (* no in-place types, right? *)
  | Struct of string (* just use name *)
      (* string * int * qbetype list (* int is alignment *) *)
*)
type qbetype =
  | Word
  | Long
  | Byte
  | Halfword
  | Single
  | Double
  | Struct of string

(*let wtype = Primitive (Integral Word)
let ltype = Primitive (Integral Long)
let btype = Primitive (Integral Byte)
let htype = Primitive (Integral Halfword)
let stype = Primitive (Floating Single)
  let dtype = Primitive (Floating Double) *)


(* Do I witness this? "type value associated with constructed value" *)
(* How I say that Integral "is-a" primitive, etc.? *)
type qbeconst =
  (* wait, can I just have one of all the int types? Then I'd just
     store the type with the value. Or it has no type in source *)
  (* | IConst: int -> (*qbeIntegral *) 't qbeconst (* might need primitive *)
     | FConst: float -> (* qbeFloating *) 't qbeconst *)
  | LConst of int64
  | WConst of int32
  | SConst of float (* no 32-bit floats in ocaml, hope for the best *)
  | DConst of float 

type qbevalue =
  | Const of qbeconst
  | Reg of qbetype * string
  | Global of qbetype * string

(* Idea: could at least use GADTs to diff lvalues *)

let q_typeof: qbevalue -> qbetype = function
  (* for consts we have to assume the largest type until we
     get a context. Then we have to check *)
  | Const (WConst _) -> Word
  | Const (LConst _) -> Long
  | Const (SConst _) -> Single
  | Const (DConst _) -> Double
  | Reg (qty, _) -> qty
  | Global (qty, _) -> qty

type qbetypeinfo =
  | Fields of (qbetype * int option) list
  | Opaque of int (* size only *)

type qbetypedef = {
  typename: string;
  align: int option;
  info: qbetypeinfo
}

(* TODO: type size function (figure out padding and packing) *)

type qbelinkage =
  | Export of string
  | Thread of string
  | Section of string * string * string


type qbedataitem =
  | Ident of string * int option (* offset - what is it? *)
  | Const of qbeconst
  | String of string

type qbedatadef = {
  linkage: qbelinkage list;
  name: string;
  align: int option;
  (* when emitting, can group items of same type without repeating type *)
  items: (string * qbedataitem) list
}

(* "Abi type". Should I make sure subword types are only used in funargs? *)
type qbefunarg =
  | Abity of qbevalue
  | Envarg of qbevalue
  | Variadic
    
(** Instructions' result values are included as the first item. They should
    only be Regs *)
type qbeinstr =
  | Add of qbevalue * qbevalue * qbevalue
  | Sub of qbevalue * qbevalue * qbevalue
  | Div of qbevalue * qbevalue * qbevalue
  | Mul of qbevalue * qbevalue * qbevalue
  | Neg of qbevalue * qbevalue
  | Udiv of qbevalue * qbevalue * qbevalue
  | Rem of qbevalue * qbevalue * qbevalue
  | Urem of qbevalue * qbevalue * qbevalue
  | Or of qbevalue * qbevalue * qbevalue
  | Xor of qbevalue * qbevalue * qbevalue
  | And of qbevalue * qbevalue * qbevalue
  | Sar of qbevalue * qbevalue * qbevalue
  | Shr of qbevalue * qbevalue * qbevalue
  | Shl of qbevalue * qbevalue * qbevalue
  (* When instructions have a type suffix, it means the type they
     take is fixed. *)
  (* Store instructions take two arguments, return nothing *)
  | Stored of qbevalue * qbevalue
  | Stores of qbevalue * qbevalue
  | Storel of qbevalue * qbevalue
  | Storew of qbevalue * qbevalue
  | Storeh of qbevalue * qbevalue
  | Storeb of qbevalue * qbevalue
  (* Loads take one argument, return value *)
  | Loadd of qbevalue * qbevalue
  | Loads of qbevalue * qbevalue
  | Loadl of qbevalue * qbevalue
  | Loadsw of qbevalue * qbevalue (* sign extend *)
  | Loaduw of qbevalue * qbevalue (* zero extend *)
  | Loadsh of qbevalue * qbevalue
  | Loaduh of qbevalue * qbevalue
  | Loadsb of qbevalue * qbevalue
  | Loadub of qbevalue * qbevalue
  (* blit can be used to copy a value-type struct, probably.
     But memcpy calls are recommended for larger data. *)
  | Blit of qbevalue * qbevalue * qbevalue
  (* The int is the alignment. Returns the address. *)
  | Alloc4 of qbevalue * qbevalue * int
  | Alloc8 of qbevalue * qbevalue * int
  | Alloc16 of qbevalue * qbevalue * int
      
  (* Comparisons - some valid only for specific types *)
  (* but same form as arithmetic *)
  | Ceqw of qbevalue * qbevalue * qbevalue
  | Ceql of qbevalue * qbevalue * qbevalue
  | Ceqs of qbevalue * qbevalue * qbevalue
  | Ceqd of qbevalue * qbevalue * qbevalue
  | Cnew of qbevalue * qbevalue * qbevalue
  | Cnel of qbevalue * qbevalue * qbevalue
  | Cnes of qbevalue * qbevalue * qbevalue
  | Cned of qbevalue * qbevalue * qbevalue
  (* These comparisons are signed or unsigned *)
  | Cslew of qbevalue * qbevalue * qbevalue
  | Cslel of qbevalue * qbevalue * qbevalue
  | Csltw of qbevalue * qbevalue * qbevalue
  | Csltl of qbevalue * qbevalue * qbevalue
  | Csgew of qbevalue * qbevalue * qbevalue
  | Csgel of qbevalue * qbevalue * qbevalue
  | Csgtw of qbevalue * qbevalue * qbevalue
  | Csgtl of qbevalue * qbevalue * qbevalue
  | Culew of qbevalue * qbevalue * qbevalue
  | Culel of qbevalue * qbevalue * qbevalue
  | Cultw of qbevalue * qbevalue * qbevalue
  | Cultl of qbevalue * qbevalue * qbevalue
  | Cugew of qbevalue * qbevalue * qbevalue
  | Cugel of qbevalue * qbevalue * qbevalue
  | Cugtw of qbevalue * qbevalue * qbevalue
  | Cugtl of qbevalue * qbevalue * qbevalue
  | Cled of qbevalue * qbevalue * qbevalue  (* float only (d and s) *)
  | Cles of qbevalue * qbevalue * qbevalue  
  | Cltd of qbevalue * qbevalue * qbevalue
  | Clts of qbevalue * qbevalue * qbevalue
  | Cged of qbevalue * qbevalue * qbevalue
  | Cges of qbevalue * qbevalue * qbevalue
  | Cgtd of qbevalue * qbevalue * qbevalue
  | Cgts of qbevalue * qbevalue * qbevalue
  | Cod of qbevalue * qbevalue * qbevalue
  | Cos of qbevalue * qbevalue * qbevalue
  | Cuod of qbevalue * qbevalue * qbevalue
  | Cuos of qbevalue * qbevalue * qbevalue
  (* Conversions *)
  (* Integer sign extension (truncation not needed) *)
  | Extsw of qbevalue * qbevalue
  | Extuw of qbevalue * qbevalue
  | Extsh of qbevalue * qbevalue
  | Extuh of qbevalue * qbevalue
  | Extsb of qbevalue * qbevalue
  | Extub of qbevalue * qbevalue
  (* Float extension and truncation *)
  | Exts of qbevalue * qbevalue
  | Truncd of qbevalue * qbevalue
  (* float-to-integer *)
  | Stosi of qbevalue * qbevalue
  | Stoui of qbevalue * qbevalue
  | Dtosi of qbevalue * qbevalue
  | Dtoui of qbevalue * qbevalue
  (* integer-to-float *)
  | Swtof of qbevalue * qbevalue
  | Uwtof of qbevalue * qbevalue
  | Sltof of qbevalue * qbevalue
  | Ultof of qbevalue * qbevalue
  (* Cast bitwise int to float*)
  | Cast of qbevalue * qbevalue
  | Copy of qbevalue * qbevalue
  (* Call-related *)
  | Call of qbevalue option * string * qbefunarg list
  | Vastart of qbevalue (* type is ignored *)
  | Vaarg of qbevalue * qbevalue
  | Phi of qbevalue * (string * int) list
  | Jmp of string
  | Jnz of qbevalue * string * string
  | Ret of qbevalue option
  | Hlt


type qbeblock = {
  label: string;
  inFunction: qbefunction;
  (* I DID IT *)
  mutable instrs: qbeinstr list (* does this let them? *)
}

and qbefunction = {
  name: string;
  linkage: qbelinkage option;
  mutable regctr: int;
  rettype: qbetype option;
  params: (qbetype * string) list;
  mutable blocks: qbeblock list
}

and qbemodule = {
  typedefs: qbetypedef list;
  (* are these the only globals? Can they be mutated? *)
  datadefs: qbedatadef list; 
  functions: qbefunction list
}


(* 'emit' functions can go here and return the result value *)
(* new lighter build-add *)
(* what do we pass in for the result? *)
(* how about just take a string and parse it if it's a reg? *)
(* or just take a string that's ONLY a reg and add the number always? *)
(* do I have to store globals? *)

let insert_instr theBlock inst =
  theBlock.instrs <- theBlock.instrs @ [inst]  

(** Takes a template for any reg-result instruction. *)
let build_reginst ifunc theBlock resname resty = 
  let theFunc = theBlock.inFunction in
  let resReg = Reg (resty, resname ^ (string_of_int theFunc.regctr)) in
  let theInst = ifunc resReg in
  (* this part can be factored out at least. *)
  (theFunc.regctr <- theFunc.regctr + 1;
   theBlock.instrs <- theBlock.instrs @ [theInst];
   resReg)

(* build functions take a string as lvalue and generate a unique register *)

let build_add theBlock resname restype v1 v2 =
  build_reginst (fun x -> Add (x, v1, v2)) theBlock resname restype

(** Type must be specified for a store because storeh and storeb take
    a word type value *)
let build_store theBlock valty v addr =
  match valty with
  | Word -> insert_instr theBlock (Storew (v, addr))
  | Long -> insert_instr theBlock (Storel (v, addr))
  | Byte -> insert_instr theBlock (Storeb (v, addr))
  | Halfword -> insert_instr theBlock (Storeh (v, addr))
  | Single -> insert_instr theBlock (Stores (v, addr))
  | Double -> insert_instr theBlock (Stored (v, addr))
  | Struct _ -> raise (BadQBE "Cannot store aggregate types")

let build_load theBlock resname valty addr =
  match valty with
  | Long -> build_reginst (fun x -> Loadl (x, addr)) theBlock resname valty
  | Single -> build_reginst (fun x -> Loads (x, addr)) theBlock resname valty
  | Double -> build_reginst (fun x -> Loadd (x, addr)) theBlock resname valty
  | Struct _ -> raise (BadQBE "Cannot load aggregate types")
  | _ -> raise (BadQBE "Short int types need loads or loadu")

(** Loads on shorter int types always store to a word. *)
let build_loadu theBlock resname valty addr =
  match valty with
  | Word -> build_reginst (fun x -> Loaduw (x, addr)) theBlock resname Word
  | Halfword -> build_reginst (fun x -> Loaduh (x, addr)) theBlock resname Word
  | Byte -> build_reginst (fun x -> Loadub (x, addr)) theBlock resname Word
  | _ -> raise (BadQBE "Invalid type for short-int load")

(* Should I combine this and the above with a flag? *)
let build_loads theBlock resname valty addr =
  match valty with
  | Word -> build_reginst (fun x -> Loadsw (x, addr)) theBlock resname Word
  | Halfword -> build_reginst (fun x -> Loadsh (x, addr)) theBlock resname Word
  | Byte -> build_reginst (fun x -> Loadsb (x, addr)) theBlock resname Word
  | _ -> raise (BadQBE "Invalid type for short-int load")


(** This builder uses the type of the first argument to select the
    specific comparison instruction. *)
let build_eq theBlock resname restype v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Ceqw (x, v1, v2)) theBlock resname restype
  | Long -> build_reginst (fun x -> Ceql (x, v1, v2)) theBlock resname restype
  | Single -> build_reginst (fun x -> Ceqs (x, v1, v2)) theBlock resname restype
  | Double -> build_reginst (fun x -> Ceqd (x, v1, v2)) theBlock resname restype
  | _ -> raise (BadQBE "Illegal type for eq comparison instruction")
 
(* ------------------- *)
(* string_of functions *)
(* ------------------- *)

let string_of_qbetype = function
  | Word -> "w"
  | Long -> "l"
  | Byte -> "b"
  | Halfword -> "h"
  | Single -> "s"
  | Double -> "d"
  | Struct name -> ":" ^ name

(* TODO: handle other arguments to Section. *)
let string_of_qbelinkage = function
  | Export nm ->
    "export " ^ (if nm <> "" then nm ^ " " else "")
  | Thread nm ->
    "thread " ^ (if nm <> "" then nm ^ " " else "")
  | Section (nm, _, _) ->
    "section " ^ (if nm <> "" then nm ^ " " else "")

(* "explicit polymorphic annotation" *)
let string_of_qbevalue : (*type t. t*) qbevalue -> string = function
  (* type markers aren't on the constant *)
  | Const (WConst n) -> Int32.to_string n
  | Const (LConst n) -> Int64.to_string n
  | Const (SConst x) -> "s_" ^ string_of_float x
  | Const (DConst x) -> "d_" ^ string_of_float x
  (* Types aren't normally printed with values *)
  | Reg (_, rname) -> "%" ^ rname
  | Global (_, gname) -> "$" ^ gname

let string_of_binop iname lval op1 op2 =
  Printf.sprintf("%s =%s %s %s, %s\n")
    (string_of_qbevalue lval)
    (string_of_qbetype (q_typeof lval))
    iname
    (string_of_qbevalue op1)
    (string_of_qbevalue op2)

let string_of_unop iname lval op =
  Printf.sprintf("%s =%s %s %s\n")
    (string_of_qbevalue lval)
    (string_of_qbetype (q_typeof lval))
    iname
    (string_of_qbevalue op)

let string_of_store iname v addr =
  Printf.sprintf("%s %s, %s")
    iname
    (string_of_qbevalue v)
    (string_of_qbevalue addr)
    

let string_of_qbeinstr = function
  | Add (lval, op1, op2) -> string_of_binop "add" lval op1 op2
  | Sub (lval, op1, op2) -> string_of_binop "sub" lval op1 op2
  | Div (lval, op1, op2) -> string_of_binop "div" lval op1 op2
  | Mul (lval, op1, op2) -> string_of_binop "mul" lval op1 op2
  | Udiv (lval, op1, op2) -> string_of_binop "udiv" lval op1 op2
  | Rem (lval, op1, op2) -> string_of_binop "rem" lval op1 op2
  | Urem (lval, op1, op2) -> string_of_binop "urem" lval op1 op2
  | Or (lval, op1, op2) -> string_of_binop "or" lval op1 op2
  | Xor (lval, op1, op2) -> string_of_binop "xor" lval op1 op2
  | And (lval, op1, op2) -> string_of_binop "and" lval op1 op2
  | Sar (lval, op1, op2) -> string_of_binop "sar" lval op1 op2
  | Shr (lval, op1, op2) -> string_of_binop "shr" lval op1 op2
  | Shl (lval, op1, op2) -> string_of_binop "shl" lval op1 op2
  | Neg (lval, op) -> string_of_unop "neg" lval op
  | Storel (v, addr) -> string_of_store "storel" v addr
  | Stores (v, addr) -> string_of_store "stores" v addr
  | Stored (v, addr) -> string_of_store "stored" v addr
  | Storew (v, addr) -> string_of_store "storew" v addr
  | Storeh (v, addr) -> string_of_store "storeh" v addr
  | Storeb (v, addr) -> string_of_store "storeb" v addr
  | Loadl (lval, addr) -> string_of_unop "loadl" lval addr
  | Loads (lval, addr) -> string_of_unop "loads" lval addr
  | Loadd (lval, addr) -> string_of_unop "loadd" lval addr
  | Loadsw (lval, addr) -> string_of_unop "loadsw" lval addr
  | Loaduw (lval, addr) -> string_of_unop "loaduw" lval addr
  | Loadsh (lval, addr) -> string_of_unop "loadsh" lval addr
  | Loaduh (lval, addr) -> string_of_unop "loaduh" lval addr
  | Loadsb (lval, addr) -> string_of_unop "loadsb" lval addr
  | Loadub (lval, addr) -> string_of_unop "loadub" lval addr
  | _ -> failwith "hold on a bit"

let string_of_qbeblock blk =
  if blk.label = "start"
  (* Actually should put this check in the insert_block function *)
  then raise (BadQBE "Label name @start reserved for function entry point")
  else 
    "@" ^ blk.label ^ "\n"

let string_of_qbefunction func =
  (match func.linkage with
   | None -> ""
   | Some lnk -> string_of_qbelinkage lnk)
  ^ "function " 
  ^ (match func.rettype with
      | Some rty -> string_of_qbetype rty
      | None -> "")
  ^ " $" ^ func.name ^ "("
  ^ String.concat ", "
    (List.map
       (fun (ty, nm) -> string_of_qbevalue (Reg (ty,nm)))
       func.params)
  ^ ") {\n"
  ^ String.concat "" (List.map string_of_qbeblock func.blocks)
  ^ "}\n"

let string_of_qbemodule = ""

(* not in first pass: "section" directive? *)

(* represent values, instructions, statements, labels, functions, data,
   typedefs,  modules *)
(* then, function to create a module *)

(* need gadt's to encode the typing rules? *)
(* don't try to typecheck myself, just get errors from qbe? *)
(* What if I use ints for any size numeric constant and it just works?
   could that produce bugs? *)

