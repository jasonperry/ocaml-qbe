
(* I don't need to model constants themselves, because they will be
   specified in the codegen directly? *)

exception BadQBE of string

type qbeIntegral =
  | Word
  | Long
  | Byte
  | Halfword

type qbeFloating =
  | Single
  | Double

type qbePrimitive =
  | Integral of qbeIntegral
  | Floating of qbeFloating

type qbetype =
  | Primitive of qbePrimitive
  | Struct of string * int * qbetype list (* int is alignment *)
                
(* type qbetype =  (* integral of, float of *)
  | Word
  | Long
  | Single
  | Double
  | Byte
  | Halfword
  (* keep all types in full description form, omit only the :name
     when needed. Good idea? *)
  | Struct of string * int * qbetype list (* int is alignment *)
*)

(* Do I witness this? "type value associated with constructed value" *)
(* How I say that Integral "is-a" primitive, etc.? *)
type _ qbeconst =
  (* wait, can I just have one of all the int types? Then I'd just
     store the type with the value. Or it has no type in source *)
  | IConst: int -> qbeIntegral qbeconst
  | FConst: float -> qbeFloating qbeconst
(*  | W of int32
  | L of int64
  (* no single floats for now *)
  | D of float
  | B of int
    | H of int *)

(* values carry their types with them (but constants?) *)
(* what about in-place struct values? *)
(* don't have to *)
type 't qbevalue =
  (* type needs to be specified per-instance.
     Multiple OCaml types can be a qbetype, so can't limit it.
     Unless there were some sort of subclass thing *)
  | Const: 't qbeconst -> 't qbevalue
  | Reg: qbetype * string -> 't qbevalue
  | Global: qbetype * string -> qbetype qbevalue (* global symbol *)

(* store the return type? Maybe not, just let it be whatever the
   result type is? Let the build function take the return type,
   and choose the instruction and/or result tag! *)
(* should the value to save go with it? instructions that save
   have to always save? Then no separate statement type. *)

type _ qbeinstr =
  (* Can I make the return a more general type? *)
  | Add: (string * qbetype * qbePrimitive qbevalue * qbePrimitive qbevalue)
      -> qbePrimitive qbeinstr
  | Ceqw: (qbeIntegral qbevalue * qbeIntegral qbevalue)
      -> qbeIntegral qbeinstr

(*
type qbeinstr =
  | Add of string * qbetype * qbevalue * qbevalue
  | Sub of string * qbetype * qbevalue * qbevalue
  | Div of string * qbetype * qbevalue * qbevalue
  | Mul of string * qbetype * qbevalue * qbevalue
  | Neg of string * qbetype * qbevalue
  | Udiv of string * qbetype * qbevalue * qbevalue
  | Rem of string * qbetype * qbevalue * qbevalue
  | Urem of string * qbetype * qbevalue * qbevalue
  | Or of string * qbetype * qbevalue * qbevalue
  | Xor of string * qbetype * qbevalue * qbevalue
  | And of string * qbetype * qbevalue * qbevalue
  | Sar of string * qbetype * qbevalue * qbevalue (* right is ww *)
  | Shr of string * qbetype * qbevalue * qbevalue
  | Shl of string * qbetype * qbevalue * qbevalue

  (* When instructions have a type suffix, it means the type they
     take is fixed. *)
  | Stored of qbevalue * qbevalue
  | Stores of qbevalue * qbevalue
  | Storel of qbevalue * qbevalue
  | Storew of qbevalue * qbevalue
  | Storeh of qbevalue * qbevalue
  | Storeb of qbevalue * qbevalue
  (* how to diff between instructions that return a value and don't? *)
  | Loadd of qbevalue
  | Loads of qbevalue
  | Loadl of qbevalue
  | Loadsw of qbevalue (* sign extend *)
  | Loaduw of qbevalue (* zero extend *)
  | Loadsh of qbevalue
  | Loaduh of qbevalue
  | Loadsb of qbevalue
  | Loadub of qbevalue
  (* blit can be used to copy a value-type struct? *)
  | Blit of qbevalue * qbevalue * qbevalue
  | Alloc4 of qbevalue
  | Alloc8 of qbevalue
  | Alloc16 of qbevalue
      
  (* Comparisons - some valid only for some types *)
  (* try to use GADTs? *)
  | Ceqw of qbevalue * qbevalue
  | Ceql of qbevalue * qbevalue
  | Ceqs of qbevalue * qbevalue
  | Ceqd of qbevalue * qbevalue
           
  | Cnew of qbevalue * qbevalue
  | Cnel of qbevalue * qbevalue
  | Cnes of qbevalue * qbevalue
  | Cned of qbevalue * qbevalue
           
  | Cslew of qbevalue * qbevalue
  | Cslel of qbevalue * qbevalue
  | Csltw of qbevalue * qbevalue
  | Csltl of qbevalue * qbevalue
  | Csgew of qbevalue * qbevalue
  | Csgel of qbevalue * qbevalue
  | Csgtw of qbevalue * qbevalue
  | Csgtl of qbevalue * qbevalue
  | Culew of qbevalue * qbevalue
  | Culel of qbevalue * qbevalue
  | Cultw of qbevalue * qbevalue
  | Cultl of qbevalue * qbevalue
  | Cugew of qbevalue * qbevalue
  | Cugel of qbevalue * qbevalue
  | Cugtw of qbevalue * qbevalue
  | Cugtl of qbevalue * qbevalue
            
  | Cled of qbevalue * qbevalue  (* float only (d and s) *)
  | Cles of qbevalue * qbevalue  
  | Cltd of qbevalue * qbevalue
  | Clts of qbevalue * qbevalue
  | Cged of qbevalue * qbevalue
  | Cges of qbevalue * qbevalue
  | Cgtd of qbevalue * qbevalue
  | Cgts of qbevalue * qbevalue
  | Cod of qbevalue * qbevalue
  | Cos of qbevalue * qbevalue
  | Cuod of qbevalue * qbevalue
  | Cuos of qbevalue * qbevalue
           *)
type 'a qbeblock = {
  label: string;
  instrs: 'a qbeinstr list
}

type 'a qbefunction = {
  name: string;
  rettype: qbetype;
  params: (qbetype * string) list;
  blocks: 'a qbeblock list
}

(* 'emit' functions can go here and return the result value *)

let string_of_qbeinstr = "" (* see sprintf *)

(* have to be explicitly universal on the type!! *)
let string_of_qbevalue : type t. t qbevalue -> string = function
  (* type markers aren't on the constant *)
  | Const (IConst n) -> string_of_int n
  | Const (FConst x) -> string_of_float x
  | Reg (_, rname) -> "%" ^ rname
  | Global (_, gname) -> "$" ^ gname

let string_of_qbeblock blk =
  if blk.label = "start"
  (* Actually should put this check in the insert_block function *)
  then raise (BadQBE "Label name @start reserved for function entry point")
  else 
    "@" ^ blk.label ^ "\n\n"

let string_of_qbemodule = ""

(* not in first pass: "section" directive? *)

(* represent values, instructions, statements, labels, functions, data,
   typedefs,  modules *)
(* then, function to create a module *)

(* need gadt's to encode the typing rules? *)
(* don't try to typecheck myself, just get errors from qbe? *)
(* What if I use ints for any size numeric constant and it just works?
   could that produce bugs? *)

