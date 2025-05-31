type 'a enode = { data : 'a; mutable children : UnionFind.id list }
type 'a egraph = UnionFind.t * (UnionFind.id, 'a enode) Hashtbl.t
