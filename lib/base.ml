type vname = string
type var = vname * int

type term =
  | V of var (* x_1 = V("x", 1) *)
  | T of string * term list (* f(a,y_2) = T ("f", [T("a",[]) , V("y",2)]) *)

type subst = (var * term) list
(* {("x",1) -> ("t",0) ; ("y",2) -> T ("f", [T("a",[]),V("y",1)])} *)

(** [indom] (p.79)

    test if x in Dom(sub) *)
let ( &? ) x (sub : subst) = List.exists (fun (y, _) -> x = y) sub

(** [app] (p.79)

    apply substitution to single variable *)
let rec ( &@ ) (sub : subst) x =
  match sub with
  | (y, t) :: _ when x = y -> t
  | _ :: dom -> dom &@ x
  | [] -> raise Not_found

(** [lift] (p.79)

    apply substitution to a term *)
let rec ( &@@ ) sub = function
  | V x -> ( try sub &@ x with Not_found -> V x)
  | T (f, t) -> T (f, List.map (fun term -> sub &@@ term) t)

(** [occurs] (fig 4.5)

    test if x in Var(t) *)
let rec ( &&? ) x = function
  | V y -> x = y
  | T (_, t) -> List.exists (fun u -> x &&? u) t

let rec offset_rename n = function
  | V (x, i) -> V (x, i + n)
  | T (f, s) -> T (f, List.map (offset_rename n) s)

let rec max_index = function
  | V (_, i) -> i
  | T (_, s) -> List.fold_left (fun m t -> max m (max_index t)) 0 s

(* UTILITIES *)

let rec term_depth t =
  match t with
  | V _ -> 1
  | T (_, ts) -> 1 + List.fold_left (fun d t -> max d (term_depth t)) 0 ts

let rec symbol_count = function
  | V _ -> 1
  | T (_, ts) -> 1 + List.fold_left (fun d t -> d + symbol_count t) 0 ts

let subscript_digit c =
  match c with
  | '0' -> "₀"
  | '1' -> "₁"
  | '2' -> "₂"
  | '3' -> "₃"
  | '4' -> "₄"
  | '5' -> "₅"
  | '6' -> "₆"
  | '7' -> "₇"
  | '8' -> "₈"
  | '9' -> "₉"
  | _ -> invalid_arg "Not a digit"

let index_to_subscript (i : int) : string =
  string_of_int i |> String.to_seq |> Seq.map subscript_digit |> List.of_seq
  |> String.concat ""

let rec string_of_term = function
  | V (x, idx) -> x ^ index_to_subscript idx
  | T (f, []) -> f
  | T (f, ts) -> f ^ "(" ^ String.concat ", " (List.map string_of_term ts) ^ ")"
