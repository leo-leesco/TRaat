module UnionFind = struct
  type 'a node = { data : 'a; parent : 'a node option; depth : int }
  (** depth should be the max number of children, 0 when a leaf *)

  let rec find n =
    match n.parent with
    | None -> n
    | Some parent -> { n with parent = Some (find parent); depth = 1 }

  let union n m =
    if n.depth = m.depth then
      { m with parent = Some { n with depth = n.depth + 1 } }
    else if n.depth < m.depth then
      { n with parent = Some { m with depth = m.depth } }
    else { m with parent = Some { n with depth = n.depth } }
end
