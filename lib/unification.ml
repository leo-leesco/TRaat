open Base

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
      (List.map (fun (u, v) -> ([ (x, t) ] &@@ u, [ (x, t) ] &@@ v)) system)
      ((x, t) :: List.map (fun (y, u) -> (y, [ (x, t) ] &@@ u)) sub)

(* try to unify both terms *)
let unify t1 t2 = solve [ (t1, t2) ] []
