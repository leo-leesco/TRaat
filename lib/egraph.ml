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
  n1.data.data = n2.data.data
  && List.for_all2
       (fun child1 child2 -> eq eg child1 child2)
       n1.data.children n2.data.children

type 'a ast = T of 'a * 'a ast list | V of 'a

let rec of_AST (eg : 'a egraph) = function
  | V x -> add eg { data = x; children = [] }
  | T (f, t) ->
      let children = List.map (fun child -> of_AST eg child) t in
      add eg { data = f; children }

let of_AST ?(eg : 'a egraph = of_list []) (term : 'a ast) =
  let idx = of_AST eg term in
  (eg, idx)
