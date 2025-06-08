open Egraph

let ( .!() ) = UnionFind.( .!() )

type var = string term

let rec string_of_ast = function
  | V x -> x
  | T (f, t) -> f ^ "(" ^ String.concat ", " (List.map string_of_ast t) ^ ")"

let () = print_endline "**** EGRAPH ****"

let () =
  print_endline "**** g(f(a,b),f(a,b)) ****";
  let term =
    T ("g", [ T ("f", [ V "a"; V "b" ]); T ("f", [ V "a"; V "b" ]) ])
  in
  let eg, g = of_term term in
  let ( =? ) = eq eg in
  print_endline (to_string eg);
  let[@warning "-8" (* suppress non exhaustive pattern warning *)] [ n1; n2 ] =
    eg.classes.!(g).children
  in
  assert (n1 =? n2)

let () = print_endline "**** f(g(a,b),h(g(a,b))) || match against f(x,y) ****"
let () = print_endline "**** f(g(a,b),h(g(a,b))) || match against h(x) ****"
let () = print_endline "**** g(a,g(b,h(c)))) || match against g(x,h(y)) ****"

let () =
  print_endline "**** f(a,b = g(c,h(d)=e)) || match against g(x,h(y)) ****"
