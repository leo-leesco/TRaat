let ( .!() ) = UnionFind.( .!() )
let ( .!()<- ) = UnionFind.( .!()<- )

type 'a quotientSet = {
  classes : 'a UnionFind.t;
  elements : ('a, UnionFind.id) Hashtbl.t;
}
(** represents abstract datasets that has equivalence classes *)

let of_list l =
  let elements = Hashtbl.create (List.length l) in
  List.iteri
    (fun idx data ->
      if Hashtbl.mem elements data then invalid_arg "duplicate entry"
      else Hashtbl.add elements data idx)
    l;
  { classes = UnionFind.create (Dynarray.of_list l); elements }

(** attempts to insert the new data into the graph and returns its [id]

    idempotent : if it already exists, simply returns the existing [id] *)
let add_elem (set : 'a quotientSet) (data : 'a) =
  try Hashtbl.find set.elements data
  with Not_found ->
    let idx = UnionFind.add set.classes data in
    Hashtbl.add set.elements data idx;
    idx

type 'a enode = { data : 'a; mutable children : UnionFind.id list }
type 'a egraph = 'a enode quotientSet

let rec eq (eg : 'a egraph) (id1 : UnionFind.id) (id2 : UnionFind.id) =
  let id1 = UnionFind.find eg.classes id1 in
  let id2 = UnionFind.find eg.classes id2 in
  id1 = id2
  ||
  let n1 = eg.classes.!(id1) in
  let n2 = eg.classes.!(id2) in
  n1.data = n2.data
  && List.for_all2
       (fun child1 child2 -> eq eg child1 child2)
       n1.children n2.children

let to_string eg =
  UnionFind.to_string
    ~string_of_a:
      (Some
         (fun { data; children } ->
           match children with
           | [] -> data
           | children ->
               data ^ "("
               ^ String.concat ", " (List.map string_of_int children)
               ^ ")"))
    eg.classes

let rec string_of_enode ~(string_of_a : 'a -> string) (eg : 'a egraph)
    (idx : UnionFind.id) =
  let node = eg.classes.!(idx) in
  string_of_a node.data
  ^
  if not (List.is_empty node.children) then
    "("
    ^ String.concat ", "
        (List.map (string_of_enode ~string_of_a eg) node.children)
    ^ ")"
  else ""

type 'a term = T of 'a * 'a term list | V of 'a

let rec add (eg : 'a egraph) = function
  | V x -> add_elem eg { data = x; children = [] }
  | T (f, t) ->
      let children = List.map (add eg) t in
      add_elem eg { data = f; children }

let of_term (term : 'a term) =
  let eg = of_list [] in
  let idx = add eg term in
  (eg, idx)

let union (eg : 'a egraph) = UnionFind.union eg.classes

type 'a substitution = ('a, UnionFind.id) Hashtbl.t
(** ['a] represents the base data stored in the [egraph] (usually a variable
    identifier such as [x] or [f]) *)

let string_of_substitution ~(string_of_a : 'a -> string) (eg : 'a egraph)
    (sub : 'a substitution) =
  "{"
  ^ String.concat ", "
      (sub |> Hashtbl.to_seq
      |> Seq.map (fun (a, idx) ->
             string_of_a a ^ " -> "
             ^ string_of_a eg.classes.!(idx).data
             ^ Base.index_to_subscript idx)
      |> List.of_seq)
  ^ "}"

module IdSet = Set.Make (Int)

(** looks for matches with [pattern] starting at [node] *)
let rec node_match (eg : 'a egraph) (subst : 'a substitution)
    (pattern : 'a term) (node : UnionFind.id) : 'a substitution list =
  let ( =? ) = eq eg in
  match pattern with
  | V x -> (
      try if node =? Hashtbl.find subst x then [ subst ] else []
      with Not_found ->
        Hashtbl.add subst x node;
        [ subst ])
  | T (f, t) ->
      if f = eg.classes.!(node).data then
        let subs =
          List.fold_left2
            (fun substitutions subpattern child ->
              List.concat
                (List.map
                   (fun subst -> enode_match eg subst subpattern child)
                   substitutions))
            [ subst ] t eg.classes.!(node).children
        in
        if subs <> [] then subs
        else
          List.concat
            (List.map
               (enode_match eg subst pattern)
               eg.classes.!(node).children)
      else []

and enode_match (eg : 'a egraph) (subst : 'a substitution) (pattern : 'a term)
    (node : UnionFind.id) : 'a substitution list =
  List.concat
    (List.map
       (node_match eg subst pattern)
       (UnionFind.get_class eg.classes node))

let rec ematch (eg : 'a egraph) (pattern : 'a term) (node : UnionFind.id) :
    'a substitution list =
  let subs = enode_match eg (Hashtbl.create 0) pattern node in
  if List.is_empty subs then
    List.concat (List.map (ematch eg pattern) eg.classes.!(node).children)
  else subs
