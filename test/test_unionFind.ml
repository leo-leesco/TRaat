open UnionFind

let () = print_endline "**** UNIONFIND ****"

let () =
  let set1 = create (Dynarray.of_seq (Seq.init 5 Fun.id)) in
  print_endline (to_string set1);

  let set2 =
    create (Dynarray.of_seq (Seq.init 6 (fun n -> n + Dynarray.length set1)))
  in
  print_endline (to_string set2);

  concat set1 set2;
  print_endline (to_string set1);
  assert (
    set1
    = create
        (Dynarray.of_seq
           (Seq.init (Dynarray.length set1 + Dynarray.length set2) Fun.id)))

let () =
  let set1 = create (Dynarray.of_seq (Seq.init 5 Fun.id)) in
  union set1 1 2;
  union set1 1 3;
  union set1 1 0;

  let set2 = create (Dynarray.of_seq (Seq.init 5 Fun.id)) in
  union set2 0 4;
  union set2 1 2;
  union set2 2 3;

  concat set1 set2;
  print_endline (to_string set1);

  let ( =? ) = eq set1 in
  assert (0 =? 1);
  assert (0 =? 2);
  assert (0 =? 3);

  (* take into account the offset induced by joining the two disjoint sets *)
  assert (5 =? 9);
  assert (6 =? 7);
  assert (6 =? 8)
