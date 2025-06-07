let ( .!() ) = UnionFind.( .!() )
let ( .!()<- ) = UnionFind.( .!()<- )

type 'a quotientSet = {
  classes : 'a UnionFind.t;
  elements : ('a, UnionFind.id) Hashtbl.t;
}
(** represents abstract datasets that has equivalence classes *)

let of_list l =
  let elements = Hashtbl.create (List.length l) in
  List.iteri
    (fun idx data ->
      if Hashtbl.mem elements data then invalid_arg "duplicate entry"
      else Hashtbl.add elements data idx)
    l;
  { classes = UnionFind.create (Dynarray.of_list l); elements }

(** attempts to insert the new data into the graph and returns its [id]

    idempotent : if it already exists, simply returns the existing [id] *)
let add (set : 'a quotientSet) (data : 'a) =
  try Hashtbl.find set.elements data
  with Not_found ->
    let idx = UnionFind.add set.classes data in
    Hashtbl.add set.elements data idx;
    idx

type 'a enode = { data : 'a; mutable children : UnionFind.id list }
type 'a egraph = 'a enode quotientSet

let rec eq (eg : 'a egraph) (id1 : UnionFind.id) (id2 : UnionFind.id) =
  let id1 = UnionFind.find eg.classes id1 in
  let id2 = UnionFind.find eg.classes id2 in
  id1 = id2
  ||
  let n1 = eg.classes.!(id1) in
  let n2 = eg.classes.!(id2) in
  n1.data = n2.data
  && List.for_all2
       (fun child1 child2 -> eq eg child1 child2)
       n1.children n2.children

type 'a term = T of 'a * 'a term list | V of 'a

let rec of_term (eg : 'a egraph) = function
  | V x -> add eg { data = x; children = [] }
  | T (f, t) ->
      let children = List.map (fun child -> of_term eg child) t in
      add eg { data = f; children }

let of_term ?(eg : 'a egraph = of_list []) (term : 'a term) =
  let idx = of_term eg term in
  (eg, idx)

type 'a substitution = ('a, UnionFind.id) Hashtbl.t
(** ['a] represents the base data stored in the [egraph] (usually a variable
    identifier such as [x] or [f]) *)

exception Incompatible

module IdSet = Set.Make (Int)

(** looks for matches with [pattern] starting at [node] *)
let rec node_match (eg : 'a egraph) (subst : 'a substitution)
    (pattern : 'a term) (node : UnionFind.id) : 'a substitution list =
  let ( =? ) = eq eg in
  match pattern with
  | V x -> (
      try if node =? Hashtbl.find subst x then [ subst ] else []
      with Not_found ->
        Hashtbl.add subst x node;
        [ subst ])
  | T (f, t) -> (
      if f <> eg.classes.!(node).data then
        let subclasses =
          List.concat
            (List.map
               (UnionFind.get_class eg.classes)
               eg.classes.!(node).children)
          |> IdSet.of_list |> IdSet.to_list (* deduplicating *)
        in
        List.concat (List.map (node_match eg subst pattern) subclasses)
      else
        match (t, eg.classes.!(node).children) with
        | pattern :: patterns, child :: children ->
            let subs = ematch eg ~subst pattern child in
            List.concat
              (List.map2
                 (fun p c ->
                   List.concat
                     (List.map (fun sub -> ematch eg ~subst:sub p c) subs))
                 patterns children)
        | [], [] -> [ subst ]
        | _ ->
            invalid_arg
              "tried to pattern match with an incompatible arity for the same \
               symbol")

and ematch (eg : 'a egraph) ?(subst : 'a substitution = Hashtbl.create 0)
    (pattern : 'a term) (node : UnionFind.id) : 'a substitution list =
  List.concat
    (List.map
       (node_match eg subst pattern)
       (UnionFind.get_class eg.classes node))
