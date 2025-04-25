
type vname = string * int

type term =
  | V of vname (* x_1 = V("x", 1) *)
  | T of string * term list (* f(a,y_2) = T ("f", [T("a",[]) , V("y",2)]) *)

type subst = (vname * term) list
(* {("x",1) -> ("t",0) ; ("y",2) -> T ("f", [T("a",[]),V("y",1)])} *)

(* test if x in Dom(sub) *)
let ( &? ) x sub = List.exists (fun (y, _) -> x = y) sub


