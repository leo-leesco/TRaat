open UnionFind

let () =
  print_newline ();
  print_endline "**** UNIONFIND ****"

let () =
  print_newline ();
  print_endline "*** test1 ***";

  let len1 = 5 in
  let set1 = create (Dynarray.of_seq (Seq.init len1 Fun.id)) in
  print_endline "set1 before concat";
  print_endline (to_string set1);

  let len2 = 6 in
  let set2 = create (Dynarray.of_seq (Seq.init len2 Fun.id)) in
  print_endline "set2";
  print_endline (to_string set2);

  concat set1 set2;
  print_endline (to_string set1);

  let extract_parent = Dynarray.map (fun { parent; _ } -> parent) in
  let print parents =
    print_endline
      ("["
      ^ String.concat ", "
          (Dynarray.to_list (Dynarray.map string_of_int parents))
      ^ "]")
  in

  print_endline "set1 after concat";
  print (extract_parent set1);

  let concatenated = create (Dynarray.of_seq (Seq.init (len1 + len2) Fun.id)) in
  print_endline "concatenated";
  print (extract_parent concatenated);

  assert (extract_parent set1 = extract_parent concatenated)

let () =
  print_newline ();
  print_endline "*** test2 ***";

  let len1 = 5 in
  let set1 = create (Dynarray.of_seq (Seq.init len1 Fun.id)) in
  union set1 1 2;
  union set1 1 3;
  union set1 1 0;

  let len2 = 6 in
  let set2 = create (Dynarray.of_seq (Seq.init len2 Fun.id)) in
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
  assert (len1 + 0 =? len1 + 4);
  assert (len1 + 1 =? len1 + 2);
  assert (len1 + 1 =? len1 + 3);

  print_endline "class of 0";
  print_endline (String.concat ", " (List.map string_of_int (get_class set1 0)));

  print_endline ("class of " ^ string_of_int (len1 + 0));
  print_endline
    (String.concat ", " (List.map string_of_int (get_class set1 (len1 + 0))));

  print_endline ("class of " ^ string_of_int (len1 + 1));
  print_endline
    (String.concat ", " (List.map string_of_int (get_class set1 (len1 + 1))))
