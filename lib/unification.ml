type vname = string * int

type term =
  | V of vname (* x_1 = V("x", 1) *)
  | T of string * term list (* f(a,y_2) = T ("f", [T("a",[]) , V("y",2)]) *)

type subst = (vname * term) list
(* {("x",1) -> ("t",0) ; ("y",2) -> T ("f", [T("a",[]),V("y",1)])} *)

(* test if x in Dom(sub) *)
let ( &? ) x sub = List.exists (fun (y, _) -> x = y) sub

(* apply substitution to single variable *)
let rec ( &@ ) sub x =
  match sub with
  | (y, t) :: _ when x = y -> t
  | _ :: dom -> dom &@ x
  | [] -> raise Not_found

(* apply substitution to a term *)
let rec ( @@ ) sub = function
  | V x -> ( try sub &@ x with Not_found -> V x)
  | T (f, t) -> T (f, List.map (fun term -> sub @@ term) t)

(* test if x in Var(t) *)
let rec ( &&? ) x = function
  | V y -> x = y
  | T (_, t) -> List.exists (fun u -> x &&? u) t

let rec solve (system : (term * term) list) (sub : subst) =
  match system with
  | [] -> sub
  | (V x, t) :: system ->
      if V x = t then solve system sub
      (* delete *) else elim x t system sub (* eliminate *)
  | (t, V x) :: system -> elim x t system sub (* orient and eliminate *)
  | (T (f, t), T (g, s)) :: system when f = g ->
      solve (List.combine t s @ system) sub (* decompose *)
  | _ -> raise (Failure "This system cannot be solved")

and elim (x : vname) (t : term) (system : (term * term) list) (sub : subst) =
  if x &&? t then raise (Failure "Trying to solve a self-referential equation")
  else
    solve
      (List.map (fun (u, v) -> ([ (x, t) ] @@ u, [ (x, t) ] @@ v)) system)
      ((x, t) :: List.map (fun (y, u) -> (y, [ (x, t) ] @@ u)) sub)

(* try to unify both terms *)
let unify t1 t2 = solve [ (t1, t2) ] []
