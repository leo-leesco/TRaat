open Basic_unification

let () =
  assert (("x", 0) &? [ (("x", 0), V ("t", 1)) ]);
  assert (not (("t", 1) &? [ (("x", 0), V ("t", 1)) ]))


