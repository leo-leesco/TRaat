type 'a enode = { data : 'a; mutable children : UnionFind.id list }

type 'a graph = {
  unionfind : UnionFind.t;
  store : 'a Dynarray.t;
  hashcons : ('a, UnionFind.id) Hashtbl.t;
}
(** [store] contains the data and is immutable, so classes are stored in
    [unionfind] *)

let of_list l =
  {
    unionfind = UnionFind.create (List.length l);
    store = Dynarray.of_list l;
    hashcons =
      Hashtbl.of_seq (List.mapi (fun idx data -> (data, idx)) l |> List.to_seq);
  }

let add (graph : 'a graph) (idx : UnionFind.id) (data : 'a) =
  UnionFind.add graph.unionfind idx;
  Dynarray.add_last graph.store data

(** [concat graph1 graph2] mutates [graph1] in-place and adds [graph2] at the
    end, preserving the parents of [graph2] logically

    in particular, the classes are still disjoint after the [concat]enation *)
let concat (graph1 : 'a graph) (graph2 : 'a graph) =
  UnionFind.concat graph1.unionfind graph2.unionfind;
  Dynarray.append graph1.store graph2.store

type 'a egraph = 'a enode graph
