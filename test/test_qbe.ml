
open Qbe

let res1 = Reg (Word, "res1")
    
let instrAdd01 = Add (res1, Const (WConst (Int32.of_int 2)),
                      Const (WConst (Int32.of_int 3)))

(* Doesn't need a return type, it's fixed *)
let res2 = Reg (Byte, "eqres1")
let instrCeqw01 = Ceqw (res2, Const (WConst (Int32.of_int 2)),
                        Const (WConst (Int32.of_int 2)))

let testfunc01: qbefunction = {
  name="myfunc";
  linkage=Some (Export "");
  regctr=1;
  rettype = Some Word;
  params=[];
  blocks=[]
}

let testblock01: qbeblock = {
  label="loop";
  inFunction=testfunc01;
  instrs=[instrAdd01;
          instrCeqw01]
}

let () = (
  testfunc01.blocks <- [testblock01];
  print_string (string_of_qbefunction testfunc01)
)

