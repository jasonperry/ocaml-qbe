(** Types and functions for emitting QBE IR code. *)

exception BadQBE of string

type qbetype =
  | Word
  | Long
  | Byte
  | Halfword
  | Single
  | Double
  | Struct of string

type qabitype =
  | W
  | L
  | UB
  | SB
  | UH
  | SH
  | S
  | D
  | St of string

let qbetype_of_qabitype = function
  | W -> Word
  | L -> Long
  | UB -> Byte
  | SB -> Byte
  | UH -> Halfword
  | SH -> Halfword
  | S -> Single
  | D -> Double
  | St tname -> Struct tname

type qbeconst =
  (* Decided to have a separate case for each constant type. *)
  | WConst of int32
  | LConst of int64
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
  | Fields of (qbetype * int) list  (* field type and count *)
  | Opaque of int (* size only *)

type qbetypedef = {
  typename: string;
  align: int option;
  info: qbetypeinfo
}

(* TODO: type size function (figure out padding and packing) *)

type qbelinkage =
  | Export
  | Thread
  | Section of string * string


type qbedataitem =
  | Ident of string * int option (* offset - for padding? *)
  | Const of qbetype * qbeconst list (* must be nonempty *)
  | String of string
  | Z of int

type qbedatadef = {
  linkage: qbelinkage list;
  name: string;
  align: int option;
  (* when emitting, can group items of same type without repeating type *)
  items: qbedataitem list
}

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
  | Alloc4 of qbevalue * qbevalue
  | Alloc8 of qbevalue * qbevalue
  | Alloc16 of qbevalue * qbevalue
      
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
  (* (return register, fname, env param, params, varargs *)
  | Call of (qabitype * string) option * string * string option
            * (qabitype * qbevalue) list * (qabitype * qbevalue) list
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
  mutable instrs: qbeinstr list (* does this let them? *)
}

and qbefunction = {
  name: string;
  linkages: qbelinkage list;
  mutable regctr: int;
  rettype: qabitype option;
  envarg: string option;
  params: (qabitype * string) list;
  vararg: bool;
  mutable blocks: qbeblock list
}

and qbemodule = {
  mutable typedefs: qbetypedef list;
  (* are these the only globals? Can they be mutated? *)
  mutable datadefs: qbedatadef list;
  mutable fundecls: qbefunction list;
  mutable fundefs: qbefunction list
}


(* ---------------------------------------------------------------- *)
(* Functions for adding program elements to the mutable structures. *)
(* ---------------------------------------------------------------- *)

(* how to copy one? Does making it a thunk make any difference? *)
let new_module () = {
  typedefs = [];
  datadefs = [];
  fundecls = [];
  fundefs = []
}

(* Okay, making a declare_function. Should I add it to the module or
   make the caller do that? LLVM adds it to the module and
   has a lookup function looks it up *)
(* I don't need to say whether the function is defined or not, I just
   put it there, it only gets emitted if I do a define! *)
let declare_function theModule fname linkages retopt 
    envopt (params: (qabitype * string) list) vararg =
  let theFunction = {
    name=fname;
    linkages=linkages;
    regctr=0;
    rettype=retopt;
    envarg=envopt;
    params=params;
    blocks=[];
    vararg=vararg
  } in
  theModule.fundecls <- theModule.fundecls @ [theFunction];
  theFunction

(** Create a block and add it to a function. *)
let add_block func blockname =
  if blockname = "start"
  then raise (BadQBE "Label name @start reserved for function entry point")
  else 
    let theBlock = {
      label=blockname;
      inFunction = func;
      instrs = []
    } in
    func.blocks <- func.blocks @ [theBlock];
    theBlock

(* Create a function to be implemented, with its start block *)
let define_function theModule fname linkages retopt envopt params vararg =
  let theFunc = declare_function
      theModule fname linkages retopt envopt params vararg in
  let startBlock = {
    label="start";
    inFunction = theFunc;
    instrs = [];
  } in
  theFunc.blocks <- [startBlock];
  theModule.fundefs <- theModule.fundefs @ [theFunc];
  theFunc
    (* theModule fname retopt params vararg =
  let theFunction =
    declare_function theModule fname [Export] retopt params vararg
  in 
       theFunction *)

let add_data theModule data =
  theModule.datadefs <- theModule.datadefs @ [data]

(* make a map later, for efficiency *)
let lookup_function theModule fname =
  List.find_opt (fun f -> f.name = fname) theModule.fundecls

let start_block func =
  List.hd func.blocks

let insert_instr theBlock inst =
  theBlock.instrs <- theBlock.instrs @ [inst]  


(* ---------------------------------------------- *)
(* Functions for building individual instructions *)
(* ---------------------------------------------- *)

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
  (* partial-word types still take a word as argument. *)
  | Byte -> insert_instr theBlock (Storeb (v, addr))
  | Halfword -> insert_instr theBlock (Storeh (v, addr))
  | Single -> insert_instr theBlock (Stores (v, addr))
  | Double -> insert_instr theBlock (Stored (v, addr))
  | Struct _ -> raise (BadQBE "Cannot store aggregate types")

(** Loads for full-size types into same size target *)
let build_load theBlock resname valty addr =
  match valty with
  | Long -> build_reginst (fun x -> Loadl (x, addr)) theBlock resname valty
  | Single -> build_reginst (fun x -> Loads (x, addr)) theBlock resname valty
  | Double -> build_reginst (fun x -> Loadd (x, addr)) theBlock resname valty
  | Struct _ -> raise (BadQBE "Cannot load aggregate types")
  | _ -> raise (BadQBE "Short int types need loads or loadu")

(* Loading shorter types into longer needs specification of sign
   behavior *)
(* Should I combine the signed and unsigned functions with a flag? *)
let build_loads theBlock resname valty resty addr =
  match valty with
  | Word -> build_reginst (fun x -> Loadsw (x, addr)) theBlock resname resty
  | Halfword -> build_reginst (fun x -> Loadsh (x, addr)) theBlock resname resty
  | Byte -> build_reginst (fun x -> Loadsb (x, addr)) theBlock resname resty
  | _ -> raise (BadQBE "Invalid type for short-int load")

let build_loadu theBlock resname resty valty addr =
  (* Can only save in a Word or Long. We can let QBE catch that mistake? *)
  match valty with
  | Word -> build_reginst (fun x -> Loaduw (x, addr)) theBlock resname resty
  | Halfword -> build_reginst (fun x -> Loaduh (x, addr)) theBlock resname resty
  | Byte -> build_reginst (fun x -> Loadub (x, addr)) theBlock resname resty
  | _ -> raise (BadQBE "Invalid type for short-int load")

let build_blit theBlock src dest len =
  let theInst = Blit (src, dest, len) in
  insert_instr theBlock theInst

(** Stack allocation; size is dynamic (a qbevalue), alignment must be 4,
    8, or 16 *)
let build_alloc theBlock resname align size =
  (* size must be a long. *)
  (* the result pointer type depends on architecture.
     Start with 64-bit hardcoded, think later about how to fix. *)
  let theInst = match align with
    | 4 -> (fun r -> Alloc4 (r, size))
    | 8 -> (fun r -> Alloc8 (r, size))
    | 16 -> (fun r -> Alloc16 (r, size))
    | _ -> raise (BadQBE "Illegal alignment size, must be 4, 8, or 16")
  in
  build_reginst theInst theBlock resname Long

(* Okay, let's try detecting the type *)
(* BUT you can use a long with a word and it's a word. Not fully general *)
let build_ceq theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Ceqw (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Ceql (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Ceqs (x, v1, v2)) theBlock resname resty
  | Double -> build_reginst (fun x -> Ceqd (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for equality comparison")

let build_cne theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Cnew (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Cnel (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Cnes (x, v1, v2)) theBlock resname resty
  | Double -> build_reginst (fun x -> Cned (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for equality comparison")

let build_csle theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Cslew (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Cslel (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for LE comparison")

let build_cslt theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Csltw (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Csltl (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for LT comparison")

let build_csge theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Csgew (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Csgel (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for GE comparison")

let build_csgt theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Csgtw (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Csgtl (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for GT comparison")


let build_cule theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Culew (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Culel (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for LE comparison")

let build_cult theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Cultw (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Cultl (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for LT comparison")

let build_cuge theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Cugew (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Cugel (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for GE comparison")

let build_cugt theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Cugtw (x, v1, v2)) theBlock resname resty
  | Long -> build_reginst (fun x -> Cugtl (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "Illegal type for GT comparison")

let build_cle theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Double -> build_reginst (fun x -> Cled (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Cles (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "cle comparison requires floating type")

let build_clt theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Double -> build_reginst (fun x -> Cltd (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Clts (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "cle comparison requires floating type")

let build_cge theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Double -> build_reginst (fun x -> Cged (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Cges (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "cle comparison requires floating type")

let build_cgt theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Double -> build_reginst (fun x -> Cgtd (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Cgts (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "cle comparison requires floating type")

let build_co theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Double -> build_reginst (fun x -> Cod (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Cos (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "ordered comparison requires floating type")

let build_cuo theBlock resname resty v1 v2 =
  match q_typeof v1 with
  | Double -> build_reginst (fun x -> Cuod (x, v1, v2)) theBlock resname resty
  | Single -> build_reginst (fun x -> Cuos (x, v1, v2)) theBlock resname resty
  | _ -> raise (BadQBE "ordered comparison requires floating type")

(** sign extension *)
let build_exts theBlock resname resty v1 =
  match q_typeof v1 with
  (* ignore resty, since we can only extend a word to Long *)
  | Word -> build_reginst (fun x -> Extsw (x, v1)) theBlock resname Long
  | Halfword -> build_reginst (fun x -> Extsh (x, v1)) theBlock resname resty
  | Byte -> build_reginst (fun x -> Extsb (x, v1)) theBlock resname resty
  (* Can only extend single to double *)
  | Single -> build_reginst (fun x -> Exts (x, v1)) theBlock resname Double
  | _ -> raise (BadQBE "illegal type for extension")

(** unsigned extension *)
let build_extu theBlock resname resty v1 =
  match q_typeof v1 with
  (* ignore resty, since we can only extend a word to Long *)
  | Word -> build_reginst (fun x -> Extuw (x, v1)) theBlock resname Long
  | Halfword -> build_reginst (fun x -> Extuh (x, v1)) theBlock resname resty
  | Byte -> build_reginst (fun x -> Extub (x, v1)) theBlock resname resty
  | _ -> raise (BadQBE "illegal type for unsigned extension")

let build_truncd theBlock resname v1 =
  (* Let QBE catch the error when there's only only type possible. *)
  build_reginst (fun x -> Truncd (x, v1)) theBlock resname Single

let build_tosi theBlock resname resty v1 =
  match q_typeof v1 with
  | Single -> build_reginst (fun x -> Stosi (x, v1)) theBlock resname resty
  | Double -> build_reginst (fun x -> Dtosi (x, v1)) theBlock resname resty
  | _ -> raise (BadQBE "tosi requires a value of floating type")

let build_toui theBlock resname resty v1 =
  match q_typeof v1 with
  | Single -> build_reginst (fun x -> Stoui (x, v1)) theBlock resname resty
  | Double -> build_reginst (fun x -> Dtoui (x, v1)) theBlock resname resty
  | _ -> raise (BadQBE "toui requires a value of floating type")

let build_stof theBlock resname resty v1 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Swtof (x, v1)) theBlock resname resty
  | Long -> build_reginst (fun x -> Sltof (x, v1)) theBlock resname resty
  | _ -> raise (BadQBE "can only convert word or long type to float")

let build_utof theBlock resname resty v1 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Uwtof (x, v1)) theBlock resname resty
  | Long -> build_reginst (fun x -> Ultof (x, v1)) theBlock resname resty
  | _ -> raise (BadQBE "can only convert word or long type to float")

let build_cast theBlock resname v1 =
  match q_typeof v1 with
  | Word -> build_reginst (fun x -> Cast (x, v1)) theBlock resname Single
  | Long -> build_reginst (fun x -> Cast (x, v1)) theBlock resname Double
  | Single -> build_reginst (fun x -> Cast (x, v1)) theBlock resname Word
  | Double -> build_reginst (fun x -> Cast (x, v1)) theBlock resname Long
  | _ -> raise (BadQBE "illegal type for cast")

(* Do I need a separate abitype for the args?
   I want the builder to not have to worry about much.
   I can convert the subword types but where do I get/keep the information
   about signed and unsigned?
   I guess there's no automatic truncation, you have to pass in a value
   of subword type if you use it. 
   Just pass in an abi type marker with everything?
   Wait...should I have a helper to build a call with a function? *)


(* Other builds don't require the module *)
(* qbe doesn't need to see definitions to build a call... *)
(* for public consumption, should I have a "raw" build_call function? *)
(* for varargs the caller needs to give the abitypes up front *)
let build_call theModule theBlock fname retopt envopt arglist varargs =
  match lookup_function theModule fname with
  | None -> raise (BadQBE ("Unknown function name " ^ fname))
  | Some func ->
    (* The "convenience" here is to insert the abitypes for you. *)
    let resopt = match retopt with
      | None -> None
      | Some rname ->
        Some (Option.get func.rettype, rname ^ string_of_int (func.regctr)) in
    let abiargs =
      List.map2 (fun arg (abity, _) -> (abity, arg)) arglist func.params in
    let theInst = Call (resopt, fname, envopt, abiargs, varargs) in
    (func.regctr <- func.regctr + 1;
     theBlock.instrs <- theBlock.instrs @ [theInst];
     (* give back a non-abitype reg. Hope that's OK *)
     Option.map (fun (abity, name) ->
         Reg (qbetype_of_qabitype abity, name)) resopt)


(* let build_funcall theBlock func areglist resname =  *)

let build_ret theBlock resopt =
  let theInstr = (Ret resopt) in
  insert_instr theBlock theInstr;
  theInstr


(* ------------------- *)
(* string_of functions *)
(* ------------------- *)

(* todo: put this above so can use it in builder error messages *)
let string_of_qbetype = function
  | Word -> "w"
  | Long -> "l"
  | Byte -> "b"
  | Halfword -> "h"
  | Single -> "s"
  | Double -> "d"
  | Struct tname -> ":" ^ tname

let string_of_qabitype = function
  | W -> "w"
  | L -> "l"
  | UB -> "ub"
  | SB -> "sb"
  | UH -> "uh"
  | SH -> "sh"
  | S -> "s"
  | D -> "d"
  | St tname -> ":" ^ tname

let string_of_qbetypedef tdef =
  "type :" ^ tdef.typename ^ " = "
  ^ (match tdef.align with
      | Some align -> "align " ^ string_of_int align
      | None -> "")
  ^ " { "
  ^ (match tdef.info with
      | Fields flist ->
        String.concat ", "
          (List.map (fun (qty, count) ->
               string_of_qbetype qty
               ^ (if count > 1 then (" " ^ string_of_int count)
                  else "")
             ) flist)
      | Opaque size -> string_of_int size)
    ^ " }"

let string_of_qbelinkage = function
  | Export -> "export"
  | Thread -> "thread"
  | Section (sname, sflags) ->
    "section"
    ^ (if sname <> "" then (" \"" ^ sname ^ "\"") else "")
    ^ (if sflags <> "" then (" \"" ^ sflags ^ "\"") else "")
       
    
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


let string_of_qbedatadef (ddef: qbedatadef) =
  (String.concat " " (List.map string_of_qbelinkage ddef.linkage))
  ^ "data $" ^ ddef.name ^ " = "
  ^ (match ddef.align with
      | Some ai -> "align " ^ string_of_int ai ^ " "
      | None -> "")
  ^ " { " ^ String.concat ", "
    (List.map (fun item -> match item with
         | Ident (ident, offopt) ->
           (* always pointer type for a named data item. *)
           "l $" ^ ident
           ^ (match offopt with
               | Some offset -> " + " ^ string_of_int offset
               | None -> "")
         | Const (ty, consts) ->
           string_of_qbetype ty ^ " "
           ^ String.concat " " (List.map (fun c ->
               string_of_qbevalue (Const c)) consts)
         (* string is always byte type, right? *)
         | String sval -> "b \"" ^ sval ^ "\""
         | Z nbytes -> "z " ^ string_of_int nbytes)
        ddef.items)
  ^ "} \n"


let string_of_qbeinstr theInstr = 
  let soi iname lvopt args =
    (match lvopt with
     | Some lval ->
       (string_of_qbevalue lval) ^ " ="
       ^ string_of_qbetype (q_typeof lval) ^ " "
     | None -> "")
    ^ iname ^ " "
    ^ String.concat ", " (List.map string_of_qbevalue args)
  in
  match theInstr with 
  | Add (lval, op1, op2) -> soi "add" (Some lval) [op1; op2]
  | Sub (lval, op1, op2) -> soi "sub" (Some lval) [op1; op2]
  | Div (lval, op1, op2) -> soi "div" (Some lval) [op1; op2]
  | Mul (lval, op1, op2) -> soi "mul" (Some lval) [op1; op2]
  | Udiv (lval, op1, op2) -> soi "udiv" (Some lval) [op1; op2]
  | Rem (lval, op1, op2) -> soi "rem" (Some lval) [op1; op2]
  | Urem (lval, op1, op2) -> soi "urem" (Some lval) [op1; op2]
  | Or (lval, op1, op2) -> soi "or" (Some lval) [op1; op2]
  | Xor (lval, op1, op2) -> soi "xor" (Some lval) [op1; op2]
  | And (lval, op1, op2) -> soi "and" (Some lval) [op1; op2]
  | Sar (lval, op1, op2) -> soi "sar" (Some lval) [op1; op2]
  | Shr (lval, op1, op2) -> soi "shr" (Some lval) [op1; op2]
  | Shl (lval, op1, op2) -> soi "shl" (Some lval) [op1; op2]
  | Neg (lval, op) -> soi "neg" (Some lval) [op]
  | Storel (v, addr) -> soi "storel" None [v; addr]
  | Stores (v, addr) -> soi "stores" None [v; addr]
  | Stored (v, addr) -> soi "stored" None [v; addr]
  | Storew (v, addr) -> soi "storew" None [v; addr]
  | Storeh (v, addr) -> soi "storeh" None [v; addr]
  | Storeb (v, addr) -> soi "storeb" None [v; addr]
  | Loadl (lval, addr) -> soi "loadl" (Some lval) [addr]
  | Loads (lval, addr) -> soi "loads" (Some lval) [addr]
  | Loadd (lval, addr) -> soi "loadd" (Some lval) [addr]
  | Loadsw (lval, addr) -> soi "loadsw" (Some lval) [addr]
  | Loaduw (lval, addr) -> soi "loaduw" (Some lval) [addr]
  | Loadsh (lval, addr) -> soi "loadsh" (Some lval) [addr]
  | Loaduh (lval, addr) -> soi "loaduh" (Some lval) [addr]
  | Loadsb (lval, addr) -> soi "loadsb" (Some lval) [addr]
  | Loadub (lval, addr) -> soi "loadub" (Some lval) [addr]
  | Blit (a1, a2, n) -> soi "blit" None [a1; a2; n]
  | Alloc4 (lval, size) -> soi "alloc4" (Some lval) [size]
  | Alloc8 (lval, size) -> soi "alloc8" (Some lval) [size]
  | Alloc16 (lval, size) -> soi "alloc16" (Some lval) [size]
  | Ceqw (res, v1, v2) -> soi "ceqw" (Some res) [v1; v2]
  | Ceql (res, v1, v2) -> soi "ceql" (Some res) [v1; v2]
  | Ceqs (res, v1, v2) -> soi "ceqs" (Some res) [v1; v2]
  | Ceqd (res, v1, v2) -> soi "ceqd" (Some res) [v1; v2]
  | Cnew (res, v1, v2) -> soi "cnew" (Some res) [v1; v2]
  | Cnel (res, v1, v2) -> soi "cnel" (Some res) [v1; v2]
  | Cnes (res, v1, v2) -> soi "cnex" (Some res) [v1; v2]
  | Cned (res, v1, v2) -> soi "cned" (Some res) [v1; v2]
  | Cslew (res, v1, v2) -> soi "cslew" (Some res) [v1; v2]
  | Cslel (res, v1, v2) -> soi "cslel" (Some res) [v1; v2]
  | Csltw (res, v1, v2) -> soi "csltw" (Some res) [v1; v2]
  | Csltl (res, v1, v2) -> soi "csltl" (Some res) [v1; v2]
  | Csgew (res, v1, v2) -> soi "csgew" (Some res) [v1; v2]
  | Csgel (res, v1, v2) -> soi "csgel" (Some res) [v1; v2]
  | Csgtw (res, v1, v2) -> soi "csgtw" (Some res) [v1; v2]
  | Csgtl (res, v1, v2) -> soi "csgtl" (Some res) [v1; v2]
  | Culew (res, v1, v2) -> soi "culew" (Some res) [v1; v2]
  | Culel (res, v1, v2) -> soi "culel" (Some res) [v1; v2]
  | Cultw (res, v1, v2) -> soi "cultw" (Some res) [v1; v2]
  | Cultl (res, v1, v2) -> soi "cultl" (Some res) [v1; v2]
  | Cugew (res, v1, v2) -> soi "cugew" (Some res) [v1; v2]
  | Cugtl (res, v1, v2) -> soi "cugtl" (Some res) [v1; v2]
  | Cled (res, v1, v2) -> soi "cled" (Some res) [v1; v2]
  | Cles (res, v1, v2) -> soi "cles" (Some res) [v1; v2]
  | Cltd (res, v1, v2) -> soi "cltd" (Some res) [v1; v2]
  | Clts (res, v1, v2) -> soi "clts" (Some res) [v1; v2]
  | Cged (res, v1, v2) -> soi "cged" (Some res) [v1; v2]
  | Cges (res, v1, v2) -> soi "cges" (Some res) [v1; v2]
  | Cgtd (res, v1, v2) -> soi "cgtd" (Some res) [v1; v2]
  | Cgts (res, v1, v2) -> soi "cgts" (Some res) [v1; v2]
  | Cod (res, v1, v2) -> soi "cod" (Some res) [v1; v2]
  | Cos (res, v1, v2) -> soi "cod" (Some res) [v1; v2]
  | Cuod (res, v1, v2) -> soi "cuod" (Some res) [v1; v2]
  | Cuos (res, v1, v2) -> soi "cuos" (Some res) [v1; v2]
  | Call (retopt, fname, envopt, params, varargs) ->
    (match retopt with
     | Some (abity, rname) ->
       "%" ^ rname ^ " =" ^ string_of_qabitype abity ^ " "
     | None -> "")
    ^ "call " ^ "$" ^ fname ^ "("
    ^ (match envopt with
        | Some ename -> "env %" ^ ename ^ ", "
        | None -> ""
      )
    ^ String.concat ", "
      (List.map (fun (abity, qval) ->
           string_of_qabitype abity ^ " " ^ string_of_qbevalue qval) params)
    ^ (if varargs = [] then ""
       else (" ... " ^ String.concat ", " (List.map (fun (abity, qval) ->
           string_of_qabitype abity ^ " " ^ string_of_qbevalue qval) varargs)))
    ^ ")"
  | Ret ropt -> soi "ret" None (Option.to_list ropt)
  | _ -> failwith "hold on a bit"


let string_of_qbeblock blk =
  "@" ^ blk.label ^ "\n"
  ^ String.concat "\n" (List.map string_of_qbeinstr blk.instrs)
  ^ "\n"

let string_of_qbefunction func =
  String.concat " " (List.map string_of_qbelinkage func.linkages)
  ^ " function " 
  ^ (match func.rettype with
      | Some rty -> string_of_qabitype rty
      | None -> "")
  ^ " $" ^ func.name ^ "("
  ^ String.concat ", "
    (List.map
       (fun (ty, nm) -> string_of_qabitype ty ^ " %" ^ nm)
       func.params)
  ^ ") {\n"
  ^ String.concat "" (List.map string_of_qbeblock func.blocks)
  ^ "}\n"

let string_of_qbemodule theMod =
  String.concat "\n" (List.map string_of_qbetypedef theMod.typedefs)
  ^ String.concat "\n" (List.map string_of_qbedatadef theMod.datadefs)
  ^ String.concat "\n" (List.map string_of_qbefunction theMod.fundefs)


(** Write a module to disk *)
let write_qbemodule modname theMod =
  let outfile = open_out (modname ^ ".ssa") in
  output_string outfile (string_of_qbemodule theMod);
  close_out outfile
