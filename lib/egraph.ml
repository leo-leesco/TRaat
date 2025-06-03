type 'a enode = { data : 'a; mutable children : UnionFind.id list }

type 'a graph = {
  unionfind : 'a UnionFind.t;
  hashcons : ('a, UnionFind.id) Hashtbl.t;
}
(** [store] contains the data and is immutable, so classes are stored in
    [unionfind] *)

let of_list l =
  let hashcons = Hashtbl.create (List.length l) in
  List.iteri
    (fun idx data ->
      if Hashtbl.mem hashcons data then invalid_arg "duplicate entry"
      else Hashtbl.add hashcons data idx)
    l;
  { unionfind = UnionFind.create (Dynarray.of_list l); hashcons }

(** attempts to insert the new data into the graph and returns its [id]

    idempotent : if it already exists, simply returns the existing [id] *)
let add (graph : 'a graph) (data : 'a) =
  try Hashtbl.find graph.hashcons data
  with Not_found ->
    let idx = UnionFind.add graph.unionfind data in
    Hashtbl.add graph.hashcons data idx;
    idx

(** [concat graph1 graph2] mutates [graph1] in-place and adds [graph2] at the
    end, preserving the parents of [graph2] logically

    in particular, the classes are still disjoint after the [concat]enation *)
let concat (graph1 : 'a graph) (graph2 : 'a graph) =
  UnionFind.concat graph1.unionfind graph2.unionfind

type 'a egraph = 'a enode graph
