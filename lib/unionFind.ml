type id = int
type elem = { mutable parent : id; mutable depth : int }

type t = elem Dynarray.t
(** represents the support of the equivalence classes, modified in-place *)

let ( .!() ) = Dynarray.get
let ( .!()<- ) = Dynarray.set
let create (n : int) : t = Dynarray.init n (fun parent -> { parent; depth = 0 })

let of_string (set : t) : string =
  "["
  ^ String.concat ", "
      (Dynarray.map (fun elem -> string_of_int elem.parent) set
      |> Dynarray.to_list)
  ^ "]"

let rec find (set : t) (elem : id) : id =
  let parent = set.!(elem).parent in
  if parent = elem then elem
  else
    let ancestor = find set parent in
    set.!(parent).parent <- ancestor;
    set.!(elem).parent <- ancestor;
    ancestor

let union (set : t) (elem1 : id) (elem2 : id) : unit =
  let repr1 = find set elem1 in
  let repr2 = find set elem2 in
  if set.!(repr1).depth > set.!(repr2).depth then set.!(repr2).parent <- repr1
  else if set.!(repr2).depth > set.!(repr1).depth then
    set.!(repr1).parent <- repr2
  else (
    set.!(repr1).parent <- repr2;
    set.!(repr2).depth <- set.!(repr2).depth + 1)

(** [concat set1 set2] mutates [set1] in-place and adds [set2] at the end,
    preserving the parents of [set2] logically

    in particular, the classes are still disjoint after the [concat]enation *)
let concat (set1 : t) (set2 : t) : unit =
  Dynarray.append set1
    (Dynarray.map
       (fun elem -> { elem with parent = elem.parent + Dynarray.length set1 })
       set2)
