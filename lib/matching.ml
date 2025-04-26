open Base

let rec match_system system sub =
  match system with
  | [] -> sub
  | (V x, t) :: system ->
      if x &? sub then
        if sub &@ x = t then match_system system sub
        else raise (Failure "Trying to solve a self-referential equation")
      else match_system system ((x, t) :: sub)
  | (_, V _) :: _ ->
      raise
        (Failure
           "No matching possible : a complex term cannot reduce to a variable")
  | (T (f, t), T (g, s)) :: system when f = g ->
      match_system (List.combine t s @ system) sub (* decompose *)
  | _ -> raise (Failure "This system cannot be solved")

let match_term pattern objective = match_system [ (pattern, objective) ] []
