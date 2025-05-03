open Matching
open Unification
open Base

exception Normal_form_reached

let rec rewrite trs t =
  match trs with
  | [] -> raise Normal_form_reached
  | (l, r) :: trs -> ( try match_term l t &@@ r with Unify _ -> rewrite trs t)

let rec norm trs t =
  match t with
  | V x -> V x
  | T (f, ts) -> (
      let u = T (f, List.map (norm trs) ts) in
      try norm trs (rewrite trs u) with Normal_form_reached -> u)
