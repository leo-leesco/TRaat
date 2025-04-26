open Base

let rec match_system system sub =
  match system with
  | [] -> sub
  | (V x, t) :: system ->
      if x &? sub then
        if sub &@ x = t then match_system system sub
        else raise (Failure "Trying to solve a self-referential equation")
      else match_system system ((x, t) :: sub)
  | (t, V x) :: _ -> raise (Failure "No matching possible (why ?)")
  | (T (f, t), T (g, s)) :: system when f = g ->
      match_system (List.combine t s @ system) sub (* decompose *)
  | _ -> raise (Failure "This system cannot be solved")

let match_term pattern objective = match_system [ (pattern, objective) ] []
