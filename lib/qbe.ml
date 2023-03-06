
(* I don't need to model constants themselves, because they will be
   specified in the codegen directly? *)

exception BadQBE of string

type qbeIntegral =
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

let wtype = Primitive (Integral Word)
let ltype = Primitive (Integral Long)
let btype = Primitive (Integral Byte)
let htype = Primitive (Integral Halfword)
let stype = Primitive (Floating Single)
let dtype = Primitive (Floating Double)


(* Do I witness this? "type value associated with constructed value" *)
(* How I say that Integral "is-a" primitive, etc.? *)
type qbeconst =
  (* wait, can I just have one of all the int types? Then I'd just
     store the type with the value. Or it has no type in source *)
  (* | IConst: int -> (*qbeIntegral *) 't qbeconst (* might need primitive *)
     | FConst: float -> (* qbeFloating *) 't qbeconst *)
  | IConst of int64
  | FConst of float (* no 32-bit floats in ocaml? Can't support them? *)

type qbelvalue
type qbervalue

(* values carry their types with them (but constants?) *)
(* what about in-place struct values? *)
(* This doesn't witness anything! The type parameter can be whatever,
   doesn't have to match the type *)
(* type _ qbevalue =
  (* type needs to be specified per-instance.
     Multiple OCaml types can be a qbetype, so can't limit it.
     Unless there were some sort of subclass thing *)
  | Const: qbeconst -> qbervalue qbevalue
  | Reg: qbetype * string -> qbelvalue qbevalue
  | Global: qbetype * string -> qbelvalue qbevalue
*)
  
type qbevalue =
  | Const of qbeconst
  | Reg of qbetype * string
  | Global of qbetype * string

(* Idea: could at least use GADTs to diff lvalues *)

let qTypeOf: qbevalue -> qbetype = function
  (* for consts we have to assume the largest type until we
     get a context. Then we have to check *)
  | Const (IConst _) -> Primitive (Integral Long)
  | Const (FConst _) -> Primitive (Floating Double)
  | Reg (qty, _) -> qty
  | Global (qty, _) -> qty
  

(* store the return type? Maybe not, just let it be whatever the
   result type is? Let the build function take the return type,
   and choose the instruction and/or result tag! *)
(* should the value to save go with it? instructions that save
   have to always save? Then no separate statement type. *)

type qbeinstr =
  | Add of qbevalue * qbevalue * qbevalue
  | Ceqw of qbevalue * qbevalue
  (* Can I make the return a more general type? *)
  (* If I remove the GADT, can I stil lhave multiple instrs of diff types? *)
(*  | Add: qbelvalue qbevalue * 'a qbevalue * 'b qbevalue
      -> qbelvalue qbeinstr
  | Ceqw: qbelvalue qbevalue * 'a qbevalue
      -> qbelvalue qbeinstr  *)


(* type _ qbeinstr =
  | Add: string * qbelvalue qbevalue * 'a qbevalue * 'b qbevalue
      -> qbelvalue qbeinstr
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

type qbelinkage =
  | Export of string option
  | Thread of string option
  | Section of string * string option * string option

type qbetypedef = {
  typename: string;
  align: int option;
  (* type is a name either way, so just a string here. *)
  fields: (string * int option) list
}

type  qbedataitem =
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

type qbeblock = {
  label: string;
  inFunction: qbefunction;
  (* I DID IT *)
  mutable instrs: qbeinstr list (* does this let them? *)
}

and qbefunction = {
  name: string;
  inModule: qbemodule;
  mutable regctr: int;
  rettype: qbetype;
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

(** build functions take a string as lvalue and generate a unique register *)
let buildAdd theBlock lval rettype v1 v2 =
  let theFunc = theBlock.inFunction in
  let lvalReg = Reg (rettype, lval ^ (string_of_int theFunc.regctr)) in
  let addInst = Add (lvalReg, v1, v2) in
  (* this part can be factored out at least. *)
  (theFunc.regctr <- theFunc.regctr + 1;
   theBlock.instrs <- theBlock.instrs @ [addInst];
   lval)


(* instruction builders *)
(* Or, I could just spit it out and let QBE tell me it's wrong. Duh. *)
(* Then build is just for adding it to the module and what I get
   back is the result value to use. *)
(* let buildAdd theMod reg v1 v2 =
  match qTypeOf v1 with
  | Struct _ ->
    raise (BadQBE "Illegal operation: cannot add struct type")
  (* How would I use the GADT info that it's primitive? *)
  | Primitive v1qty -> (
      match qTypeOf v2 with
      | Struct _ -> 
        raise (BadQBE "Illegal operation: cannot add struct type")
      | Primitive v2qty ->
        if v1qty <> v2qty then
          if (v1qty == wtype && v2qty == ltype
              || v1qty == ltype && v2qty == wtype) then
            (* oh, what if I store it in a global? *)
            (theMod.regctr <- theMod.regctr + 1; 
             Add (reg ^ string_of_int theMod.regctr, wtype, v1, v2))
          else
            (* No wait, constants can be interpreted as any size *)
            raise (BadQBE "Incompatible types for add")
        else
          (theMod.regctr <- theMod.regctr + 1; 
           Add (reg ^ string_of_int theMod.regctr, v1qty, v1, v2)))
*)

(* string_of functions *)


let string_of_qbeinstr = "" (* see sprintf *)

(* "explicit polymorphic annotation" *)
let string_of_qbevalue : (*type t. t*) qbevalue -> string = function
  (* type markers aren't on the constant *)
  | Const (IConst n) -> Int64.to_string n
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

