
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
  linkages=[Export];
  regctr=1;
  rettype = Some W;
  envarg = None;
  params=[];
  vararg=false;
  blocks=[]
}

let testblock01: qbeblock = {
  label="loop";
  inFunction=testfunc01;
  instrs=[instrAdd01;
          instrCeqw01]
}

let testModule01: qbemodule =
  let theMod = new_module() in
  let main = define_function theMod "main" [Export] (Some W) None [] false in
  let strdata = {
    linkage = [];
    name = "strdata";
    align = None;
    items = [String "Hello World"; Constdata (Byte, [WConst Int32.zero])]
  } in
  add_data theMod strdata;
  (* hmmm, export is meaningless with declare. *)
  (ignore
     (declare_function theMod "puts" [Export] (Some W) None [(L, "s")] true);
   (* let puts = Option.get (lookup_function theMod "puts") in *)
   (* let putRes = *)
   ignore (build_call theMod (start_block main) "puts" (Some "stat") None
             [Global (Long, "strdata")] []);
   ignore (build_ret (start_block main) (Some (Const (WConst Int32.zero))));
   theMod)
  

let () = (
  testfunc01.blocks <- [testblock01];
  (* print_string (string_of_qbefunction testfunc01); *)
  (* print_string (string_of_qbemodule testModule01) *)
  write_qbemodule "hello" testModule01
)

