
open Qbe

let res1 = Reg (wtype, "res1")
    
let instrAdd01 = Add (res1, Const (IConst 2L), Const (IConst 3L))

(* Doesn't need a return type, it's fixed *)
let res2 = Reg (btype, "eqres1")
let instrCeqw01 = Ceqw (res2, Const (IConst 2L))

let testfunc01: qbefunction = {
  name="myfunc";
  regctr=1;
  rettype = wtype;
  params=[];
  blocks=[]
}

let testblock01: qbeblock = {
  label="loop";
  inFunction=testfunc01;
  instrs=[instrAdd01;
          instrCeqw01]
}

let () = print_string (string_of_qbeblock testblock01)

