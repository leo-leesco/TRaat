type id = int
type 'a elem = { data : 'a; mutable parent : id; mutable depth : int }

type 'a t = 'a elem Dynarray.t
(** represents the support of the equivalence classes, modified in-place *)

let ( .!() ) = Dynarray.get
let ( .!()<- ) = Dynarray.set

let create (data : 'a Dynarray.t) : 'a t =
  Dynarray.mapi (fun parent data -> { data; parent; depth = 0 }) data

let to_string ?(string_of_a : ('a -> string) option = None) ?(index = true)
    (set : 'a t) : string =
  "["
  ^ String.concat ", "
      (Dynarray.mapi
         (fun idx elem ->
           (if index then string_of_int idx ^ ":" else "")
           ^
           match string_of_a with
           | Some to_string ->
               to_string elem.data ^ " -> " ^ to_string set.!(elem.parent).data
           | None -> string_of_int elem.parent)
         set
      |> Dynarray.to_list)
  ^ "]"

let rec find (set : 'a t) (elem : id) : id =
  let parent = set.!(elem).parent in
  if parent = elem then elem
  else
    let ancestor = find set parent in
    set.!(elem).parent <- ancestor;
    ancestor

let eq (set : 'a t) (elem1 : id) (elem2 : id) : bool =
  find set elem1 = find set elem2

let union (set : 'a t) (elem1 : id) (elem2 : id) : unit =
  let repr1 = find set elem1 in
  let repr2 = find set elem2 in
  if set.!(repr1).depth > set.!(repr2).depth then set.!(repr2).parent <- repr1
  else if set.!(repr2).depth > set.!(repr1).depth then
    set.!(repr1).parent <- repr2
  else (
    set.!(repr1).parent <- repr2;
    set.!(repr2).depth <- set.!(repr2).depth + 1)

let add (set : 'a t) (data : 'a) =
  let idx = Dynarray.length set in
  Dynarray.add_last set { data; parent = idx; depth = 0 };
  idx

(** [concat set1 set2] mutates [set1] in-place and adds [set2] at the end,
    preserving the parents of [set2] logically

    in particular, the classes are still disjoint after the [concat]enation *)
let concat (set1 : 'a t) (set2 : 'a t) : unit =
  Dynarray.append set1
    (Dynarray.map
       (fun elem -> { elem with parent = elem.parent + Dynarray.length set1 })
       set2)

(** returns the [id list] of elements in the class of the queried [id] *)
let get_class (set : 'a t) (repr : id) =
  let in_class = eq set repr in
  List.filter in_class (List.of_seq (Seq.init (Dynarray.length set) Fun.id))

(** access [data] only *)
let ( .!() ) set idx = set.!(idx).data

(** modify [data] only *)
let ( .!()<- ) set idx data = set.!(idx) <- data
