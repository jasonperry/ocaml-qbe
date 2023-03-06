
open Qbe

let instrAdd01 = Add ("res1", (Primitive (Integral Word)),
                      Const (IConst 2), Const (IConst 3))

(* Doesn't need a return type, it's fixed *)
let instrCeqw01 = Ceqw ("eqres1", Const (IConst 2), Const (IConst 3))

let testfunc01: qbefunction = {
  name="myfunc";
  regctr=1;
  rettype = 

let testblock01: qbeblock = {
  label="loop";
  instrs=[instrAdd01;
          instrCeqw01]
}

let () = print_string (string_of_qbeblock testblock01)

