open Base
open OUnit2

let () =
  print_newline ();
  print_endline "**** BASE ****"

let () =
  print_endline (string_of_term (V ("t", 1)));
  print_endline
    (string_of_term (T ("g", [ T ("f", [ V ("x", 0); V ("x", 1) ]) ])))

let () =
  assert (("x", 0) &? [ (("x", 0), V ("t", 1)) ]);
  assert (not (("t", 1) &? [ (("x", 0), V ("t", 1)) ]))

let () =
  assert ([ (("x", 0), V ("t", 1)) ] &@ ("x", 0) = V ("t", 1));
  assert_raises Not_found (fun () -> [ (("x", 0), V ("t", 1)) ] &@ ("t", 1))

let () =
  assert (3 = term_depth (T ("g", [ T ("f", [ V ("x", 0); V ("x", 1) ]) ])));

  assert (4 = symbol_count (T ("g", [ T ("f", [ V ("x", 0); V ("x", 1) ]) ])))
