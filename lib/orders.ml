open Base

type ord = LT | EQ | GT
type 'a order = 'a * 'a -> ord

let rec lex (ord : 'a order) = function
  | l, m when List.length l <> List.length m ->
      if List.length l > List.length m then GT else LT
  | a :: l, b :: m -> if ord (a, b) = EQ then lex ord (l, m) else ord (a, b)
  | [], [] -> EQ
  | [], _ | _, [] ->
      failwith
        "element-wise comparison should not be performed on different length \
         lists"

let rec lex_path_order (ord : string order) = function
  | s, (V x as t) -> if s = t then EQ else if x &&? s then GT (* LPO1 *) else LT
  | V _, T _ -> LT
  | (T (f, sl) as s), (T (g, tl) as t) ->
      (* LPO2 *)
      if List.for_all (fun si -> lex_path_order ord (si, t) = LT) sl then
        match ord (f, g) with
        | GT ->
            if List.for_all (fun ti -> lex_path_order ord (s, ti) = GT) tl then
              GT (* LPO2b *)
            else LT
        | EQ ->
            if List.for_all (fun ti -> lex_path_order ord (s, ti) = GT) tl then
              lex (lex_path_order ord) (sl, tl) (* LPO2c *)
            else LT
        | LT -> LT
      else GT (* LPO2a *)

let rec rec_path_order
    (status : string -> term order -> term list * term list -> ord)
    (ord : string order) = function
  | s, (V x as t) -> if s = t then EQ else if x &&? s then GT (* LPO1 *) else LT
  | V _, T _ -> LT
  | (T (f, sl) as s), (T (g, tl) as t) ->
      if List.for_all (fun si -> rec_path_order status ord (si, t) = LT) sl then
        match ord (f, g) with
        | GT ->
            if
              List.for_all (fun ti -> rec_path_order status ord (s, ti) = GT) tl
            then GT
            else LT
        | EQ ->
            if
              List.for_all (fun ti -> rec_path_order status ord (s, ti) = GT) tl
            then (status f) (rec_path_order status ord) (sl, tl)
            else LT
        | LT -> LT
      else GT

(** returns a positive function over terms, induced by a weight function over
    variables and function symbols

    it should be admissible when used with an order (for the
    {!knuth_bendix_order}) *)
let term_weight (w : vname -> float) = failwith "todo"

let rec knuth_bendix_order (ord : term order) (w : term -> float) = ()
