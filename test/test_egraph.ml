open Egraph

let ( .!() ) = UnionFind.( .!() )

type var = string term

let rec string_of_ast = function
  | V x -> x
  | T (f, t) -> f ^ "(" ^ String.concat ", " (List.map string_of_ast t) ^ ")"

let () =
  print_newline ();
  print_endline "**** EGRAPH ****"

let () =
  print_newline ();
  print_endline "*** g(f(a,b),f(a,b)) ***";
  let term =
    T ("g", [ T ("f", [ V "a"; V "b" ]); T ("f", [ V "a"; V "b" ]) ])
  in
  let eg, g = of_term term in
  let ( =? ) = eq eg in
  print_endline (to_string eg);
  print_endline (string_of_enode ~string_of_a:Fun.id eg g);

  let[@warning "-8" (* suppress non exhaustive pattern warning *)] [ n1; n2 ] =
    eg.classes.!(g).children
  in
  assert (n1 =? n2)

let () =
  print_newline ();
  print_endline "*** f(g(a,b),h(g(a,b))) || match against f(x,y) ***";
  let term =
    T
      ( "f",
        [ T ("g", [ V "a"; V "b" ]); T ("h", [ T ("g", [ V "a"; V "b" ]) ]) ] )
  in
  let eg, f = of_term term in
  print_endline (to_string eg);

  let pattern = T ("f", [ V "x"; V "y" ]) in
  let matches = ematch eg pattern f in
  print_endline (string_of_enode ~string_of_a:Fun.id eg f);
  print_endline
    ("matches :\n"
    ^ String.concat "\n"
        (List.map (string_of_substitution eg ~string_of_a:Fun.id) matches))

let () =
  print_newline ();
  print_endline "*** f(g(a,b),h(g(a,b))) || match against h(x) ***";
  let term =
    T
      ( "f",
        [ T ("g", [ V "a"; V "b" ]); T ("h", [ T ("g", [ V "a"; V "b" ]) ]) ] )
  in
  let eg, f = of_term term in
  print_endline (to_string eg);

  let pattern = T ("h", [ V "x" ]) in
  let matches = ematch eg pattern f in
  print_endline (string_of_enode ~string_of_a:Fun.id eg f);
  print_endline
    ("matches :\n"
    ^ String.concat "\n"
        (List.map (string_of_substitution eg ~string_of_a:Fun.id) matches));

  print_newline ();
  print_endline
    "** h(g(a,b)) (inside f(g(a,b),h(g(a,b)))) || match against h(x) **";
  let h_index = 3 in
  print_endline eg.classes.!(h_index).data;
  let matches_from_h = ematch eg pattern h_index in
  print_endline (string_of_enode ~string_of_a:Fun.id eg f);
  print_endline
    ("matches :\n"
    ^ String.concat "\n"
        (List.map
           (string_of_substitution eg ~string_of_a:Fun.id)
           matches_from_h))

let () =
  print_newline ();
  print_endline "*** g(a,g(b,h(c)))) || match against g(x,h(y)) ***";
  let term = T ("g", [ V "a"; T ("g", [ V "b"; T ("h", [ V "c" ]) ]) ]) in
  let eg, g0 = of_term term in
  print_endline (to_string eg);

  let pattern = T ("g", [ V "x"; T ("h", [ V "y" ]) ]) in
  let matches = ematch eg pattern g0 in
  print_endline (string_of_enode ~string_of_a:Fun.id eg g0);
  print_endline
    ("matches :\n"
    ^ String.concat "\n"
        (List.map (string_of_substitution eg ~string_of_a:Fun.id) matches))

let () =
  print_newline ();
  print_endline "*** f(a,b = g(c,h(d)=e)) || match against g(x,h(y)) ***";
  let fab = T ("f", [ V "a"; V "b" ]) in
  let gch = T ("g", [ V "c"; T ("h", [ V "d" ]) ]) in
  let e = V "e" in
  let eg, f = of_term fab in
  print_endline (to_string eg);
  ()
