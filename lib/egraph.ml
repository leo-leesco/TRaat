module UnionFind = struct
  type 'a node = { data : 'a; parent : 'a node option; depth : int }
  (** depth should be the max number of children, 0 when a leaf

      parent points to itself when it is the ancestor of the class

      (not reduced by path compression, so it is actually worst case) *)

  let node data = { data; parent = None; depth = 0 }
  let eq a b = a.data = b.data

  (** returns the updated node with its parent being the ancestor of the class
  *)
  let rec find n =
    match n.parent with
    | None -> None
    | Some parent -> Some { parent with parent = find parent }
  (* if n.parent = n then n else { n.parent with parent = find n.parent } *)

  let union n m =
    let n = Option.value (find n) ~default:n in
    let m = Option.value (find m) ~default:m in

    if n.depth > m.depth then
      { m with parent = Some { n with depth = n.depth } }
    else if n.depth < m.depth then
      { n with parent = Some { m with depth = m.depth } }
    else { m with parent = Some { n with depth = n.depth + 1 } }
end

type 'a enode = { data : 'a UnionFind.node; parent : 'a enode option }
