open Base
open Unification

let critical_pair (context : term -> term) ((t, r) : term * term)
    ((l2, r2) : term * term) =
  try
    let sub = fun s -> unify t l2 &@@ s in
    [ (sub r, sub (context r2)) ]
  with Unify _ -> []

let critical_pairs trs (l, r) =
  let rec critical_pairs context = function
    | V _, _ -> []
    | (T (f, s), r) as t ->
        List.concat (List.map (critical_pair context t) trs)
        @ inner_critical_pairs context (f, [], s, r)
  and inner_critical_pairs context = function
    | _, _, [], _ -> []
    | f, s0, t :: s1, r ->
        let context_f s = context (T (f, s0 @ [ s ] @ s1)) in
        critical_pairs context_f (t, r)
        @ inner_critical_pairs context (f, s0 @ [ t ], s1, r)
  in
  let m =
    1
    + List.fold_left
        (fun mindex (l, r) -> max (max (max_index l) (max_index r)) mindex)
        0 trs
  in

  critical_pairs Fun.id (offset_rename m l, offset_rename m r)

let critical_pairs2 trs1 trs2 =
  List.concat (List.map (critical_pairs trs1) trs2)

let critical_pairs trs = critical_pairs2 trs trs
