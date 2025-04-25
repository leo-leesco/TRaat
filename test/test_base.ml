open Base
open OUnit2

let () =
  assert (("x", 0) &? [ (("x", 0), V ("t", 1)) ]);
  assert (not (("t", 1) &? [ (("x", 0), V ("t", 1)) ]))

let () =
  assert ([ (("x", 0), V ("t", 1)) ] &@ ("x", 0) = V ("t", 1));
  assert_raises Not_found (fun () -> [ (("x", 0), V ("t", 1)) ] &@ ("t", 1))
