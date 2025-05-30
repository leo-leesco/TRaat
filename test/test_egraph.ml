module UnionFind = struct
  open Egraph.UnionFind

  let to_string (graph : string classes) =
    String.concat ",\n"
      (map (fun parent data -> data ^ " -> " ^ string_of_idx parent) graph)

  let graph = make [ "a"; "b"; "c"; "d" ]

  let () =
    print_endline (to_string graph);
    let _ = union graph 0 1 in
    let _ = union graph 0 2 in
    print_endline (to_string graph)
end
