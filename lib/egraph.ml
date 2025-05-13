module UnionFind = struct
  type 'a node = { data : 'a; mutable parent : int; mutable depth : int }
  (** depth should be the max number of children, 0 when a leaf

      (not reduced by path compression, so it is actually worst case) *)

  type 'a graph = 'a node Array.t

  let make data_array =
    Array.mapi (fun idx data -> { data; parent = idx; depth = 0 }) data_array

  (** returns the index of the ancestor

      performs path compression *)
  let rec find graph node_idx =
    if not (0 <= node_idx && node_idx <= Array.length graph) then
      raise (Invalid_argument "Tried to access an out-of-bounds node")
    else if graph.(node_idx).parent <> node_idx then (
      graph.(node_idx).parent <- find graph node_idx;
      graph.(node_idx).parent)
    else node_idx

  let union graph n m =
    let n = find graph n in
    let m = find graph m in

    if graph.(n).depth > graph.(m).depth then graph.(m).parent <- n
    else if graph.(n).depth < graph.(m).depth then graph.(n).parent <- m
    else (
      graph.(m).parent <- n;
      graph.(n).depth <- graph.(n).depth + 1)
end

type 'a enode = { data : 'a UnionFind.node; parent : 'a enode option }
