module UnionFind = struct
  open Egraph.UnionFind

  (* let rec to_string = function *)
  (*   | None -> "" *)
  (*   | Some n -> *)
  (*       String.concat "\n" *)
  (*         ((string_of_int n.data ^ "; depth:" ^ string_of_int n.depth) *)
  (*         :: List.map *)
  (*              (fun line -> "\t" ^ line) *)
  (*              (String.split_on_char '\n' (to_string n.parent))) *)

  let rec to_string n =
    (match n.parent with
    | None -> "___\n"
    | Some parent -> to_string parent ^ "\n^\n")
    ^ n.data ^ "; depth:" ^ string_of_int n.depth

  let () =
    let a = node "a" in
    let b = node "b" in
    let c = node "c" in
    let d = node "d" in

    let b = union a b in
    let c = union a c in
    let d = union b d in
    let a = union b c in

    print_endline (to_string a);
    print_endline (to_string b);
    print_endline (to_string c);
    print_endline (to_string d);

    (* assert (eq (find ab) (find ac)); *)
    (* assert (eq (find bd) (find ac)); *)
    ()
end
