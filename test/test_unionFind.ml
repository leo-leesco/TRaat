open UnionFind

let () =
  let classes = create 5 in
  print_endline (of_string classes);
  let set = create 6 in
  concat classes set;
  print_endline (of_string classes);
  assert (classes = create 11)
