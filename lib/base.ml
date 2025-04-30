type vname = string * int

type term =
  | V of vname (* x_1 = V("x", 1) *)
  | T of string * term list (* f(a,y_2) = T ("f", [T("a",[]) , V("y",2)]) *)

type subst = (vname * term) list
(* {("x",1) -> ("t",0) ; ("y",2) -> T ("f", [T("a",[]),V("y",1)])} *)

(** indom (p.79) * test if x in Dom(sub) *)
let ( &? ) x sub = List.exists (fun (y, _) -> x = y) sub

(** app (p.79) * apply substitution to single variable *)
let rec ( &@ ) sub x =
  match sub with
  | (y, t) :: _ when x = y -> t
  | _ :: dom -> dom &@ x
  | [] -> raise Not_found

(** lift (p.79) * apply substitution to a term *)
let rec ( &@@ ) sub = function
  | V x -> ( try sub &@ x with Not_found -> V x)
  | T (f, t) -> T (f, List.map (fun term -> sub &@@ term) t)

(** occurs (fig 4.5) * test if x in Var(t) *)
let rec ( &&? ) x = function
  | V y -> x = y
  | T (_, t) -> List.exists (fun u -> x &&? u) t
