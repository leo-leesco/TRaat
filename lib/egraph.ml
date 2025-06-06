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

type substitution = (UnionFind.id, UnionFind.id) Hashtbl.t
(** specifies a node (intended to be a leaf i.e. a variable) to be replaced by a
    tree

    both are designated by their [id] *)

exception Incompatible

(** looks for matches with [pattern] starting at [node] *)
let rec ematch (eg : 'a egraph) ~(node : UnionFind.id) (subst : substitution) :
    'a term -> unit =
  let ( =? ) = eq eg in
  function
  | V x -> (
      try
        let idx = Hashtbl.find eg.elements { data = x; children = [] } in
        try
          let replace = Hashtbl.find subst idx in
          if replace =? node then () else raise Incompatible
        with Not_found -> Hashtbl.add subst node idx
      with Not_found ->
        let _, idx = of_term ~eg (V x) in
        Hashtbl.add subst node idx)
  | T (f, t) ->
      if eg.classes.!(node).data.data <> f then raise Incompatible
      else
        let get_class = UnionFind.get_class eg.classes in
        List.iter2
          (fun subterm node ->
            List.iter
              (fun node -> ematch eg ~node subst subterm)
              (get_class node))
          t eg.classes.!(node).data.children
