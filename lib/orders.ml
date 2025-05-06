open Base

(** [lex l m] returns the sign of "[l - m]", i.e.

    [l<m ->] negative

    [l=m ->] 0

    [l>m ->] positive *)
let rec lex (ord : 'a -> 'a -> int) = function
  | l, m when List.length l <> List.length m -> List.length l - List.length m
  | a :: l, b :: m -> if ord a b = 0 then lex ord (l, m) else ord a b
  | [], [] -> 0
  | [], _ | _, [] ->
      failwith "element-wise comparison should have already be handled"

let rec lex_path_order (ord : string -> string -> int) = function
  | s, (V x as t) -> if s = t then 0 else if x &&? s then 1 (* LPO1 *) else -1
  | V _, T _ -> -1
  | (T (f, sl) as s), (T (g, tl) as t) ->
      (* LPO2 *)
      if List.for_all (fun si -> lex_path_order ord (si, t) < 0) sl then
        let sg = ord f g in
        if sg > 0 then
          if List.for_all (fun ti -> lex_path_order ord (s, ti) > 0) tl then 1
            (* LPO2b *)
          else -1
        else if sg = 0 then
          if List.for_all (fun ti -> lex_path_order ord (s, ti) > 0) tl then
            lex (fun s t -> lex_path_order ord (s, t)) (sl, tl) (* LPO2c *)
          else -1
        else -1
      else 1 (* LPO2a *)

(** generalizes [lex_path_order] and [multiset_path_order] using a dynamic
    [status] parameter

    [status f] is the order used for the term [f(s1…sn)], which means it can be
    dynamically chosen based on the function used*)
let rec rec_path_order
    ~(status : string -> (term -> term -> int) -> term list * term list -> int)
    (ord : string -> string -> int) = function
  | s, (V x as t) -> if s = t then 0 else if x &&? s then 1 (* LPO1 *) else -1
  | V _, T _ -> -1
  | (T (f, sl) as s), (T (g, tl) as t) ->
      if List.for_all (fun si -> rec_path_order ~status ord (si, t) < 0) sl then
        let sg = ord f g in
        if sg > 0 then
          if List.for_all (fun ti -> rec_path_order ~status ord (s, ti) > 0) tl
          then 1
          else -1
        else if sg = 0 then
          if List.for_all (fun ti -> rec_path_order ~status ord (s, ti) > 0) tl
          then (status f) (fun s t -> rec_path_order ~status ord (s, t)) (sl, tl)
          else -1
        else -1
      else 1

(** returns a positive function over terms, induced by a weight function over
    variables and function symbols

    it should be admissible when used with an order (for the
    {!knuth_bendix_order}) *)
let term_weight (w : vname -> float) = failwith "TODO"

let rec knuth_bendix_order (ord : term -> term -> int) (w : term -> float) =
  failwith "TODO"
