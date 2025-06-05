open Egraph

let ( .!() ) = UnionFind.( .!() )

type var = string term

let rec string_of_ast = function
  | V x -> x
  | T (f, t) -> f ^ "(" ^ String.concat ", " (List.map string_of_ast t) ^ ")"

let () = print_endline "**** EGRAPH ****"

let () =
  let term =
    T ("g", [ T ("f", [ V "a"; V "b" ]); T ("f", [ V "a"; V "b" ]) ])
  in
  let eg, idx = of_term term in
  let ( =? ) = eq eg in
  print_endline
    (UnionFind.to_string
       ~string_of_a:
         (Some
            (fun { data; children } ->
              match children with
              | [] -> data
              | children ->
                  data ^ "("
                  ^ String.concat ", " (List.map string_of_int children)
                  ^ ")"))
       eg.classes);
  let[@warning "-8" (* suppress non exhaustive pattern warning *)] [ n1; n2 ] =
    eg.classes.!(idx).data.children
  in
  assert (n1 =? n2)
