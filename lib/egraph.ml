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

let concat (eg1 : 'a egraph) (eg2 : 'a egraph) =
  UnionFind.concat eg1.classes eg2.classes
