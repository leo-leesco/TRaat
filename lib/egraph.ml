(** represents equivalence classes as connected components of a graph *)
module UnionFind : sig
  type id
  (** should be a valid index i.e. be lower than the number of elements in the
      set *)

  type 'a graph

  val ( .!() ) : 'a graph -> id -> 'a
  (** access the data contained inside a node *)

  val make : 'a list -> 'a graph
  val add : 'a graph -> 'a -> unit
  val find : 'a graph -> id -> id
  val union : 'a graph -> id -> id -> unit

  val join : ?f:(int -> 'a -> 'a) -> 'a graph -> 'a graph -> 'a graph
  (** merge two unconnected sets together and applies [f] to the second one

      equivalence classes are preserved

      [f] first argument is the offset, in case [data] relies on [id] *)
end = struct
  type id = int

  type 'a node = { data : 'a; mutable parent : id; mutable depth : id }
  (** [depth] should be the max number of children, 0 when a leaf

      [parent] is the index in the array

      (not reduced by path compression, so it is actually worst case) *)

  type 'a graph = 'a node Dynarray.t

  (* those serve to mimic the interface of [Array] *)
  let ( .!() ) = Dynarray.get
  (* let ( .!()<- ) = Dynarray.set *)

  let make data =
    Dynarray.mapi
      (fun idx data -> { data; parent = idx; depth = 0 })
      (Dynarray.of_list data)

  let add (graph : 'a graph) data =
    Dynarray.add_last graph { data; parent = Dynarray.length graph; depth = 0 }

  let assert_valid_id (graph : 'a graph) (node_idx : id) =
    if not (0 <= node_idx && node_idx <= Dynarray.length graph) then
      raise (Invalid_argument "Tried to access an out-of-bounds node")

  (** returns the index of the ancestor

      performs path compression *)
  let rec find (graph : 'a graph) node_idx =
    assert_valid_id graph node_idx;

    if graph.!(node_idx).parent <> node_idx then (
      graph.!(node_idx).parent <- find graph node_idx;
      graph.!(node_idx).parent)
    else node_idx

  let union graph n m =
    let n = find graph n in
    let m = find graph m in

    if graph.!(n).depth > graph.!(m).depth then graph.!(m).parent <- n
    else if graph.!(n).depth < graph.!(m).depth then graph.!(n).parent <- m
    else (
      graph.!(m).parent <- n;
      graph.!(n).depth <- graph.!(n).depth + 1)

  (* we only allow to access data, not the internal implementation *)
  let ( .!() ) graph idx = graph.!(idx).data

  let join ?(f = fun _ data -> data) graph1 graph2 =
    let offset = Dynarray.length graph1 in
    Dynarray.append graph1 graph2;
    Dynarray.mapi
      (fun idx elem ->
        if idx > offset then
          { elem with parent = elem.parent + offset; data = f offset elem.data }
        else elem)
      graph1
end

open UnionFind

type 'a enode = { data : 'a; children : id list }
type 'a eclass = 'a enode list
type 'a egraph = 'a eclass graph

(* What should an e-graph do ?
 * 1. load the AST of an expression
 * 2. apply a given rewrite to it
 * 3. extract the best rewritten equivalent expression according to some cost function
 *
 * What is specific about an e-graph ?
 * - each e-node points to e-classes, but e-classes do not point to anything
 * - rewrites are applied on a match with a sought after pattern and add new e-nodes (and connections based on the match)
 * - when adding e-nodes, try to share with existing ones (has to be the same data and children)
 *)

let of_term (expr : Base.term) : Base.vname egraph = failwith "TODO"

(** implements extraction based on cost function

    if the cost function is not provided, simply find the lightest equivalent
    term (each edge gets weigthed equally) *)
let to_term ?(weight = None) (eg : Base.vname egraph) : Base.term =
  failwith "TODO"

(** apply in place the rewrite to the egraph *)
let ( @@= ) rewrite (eg : Base.vname egraph) = failwith "TODO"

(* type 'var subst = 'var -> id *)
(* (** WARNING : this is not [Base.subst] *)
(**)
(*     [Base.subst] is a list of substitutions that maps variables to terms *)
(**)
(*     [Egraph.subst] returns the id of an enode that matches [!TODO what ?] *) *)
(**)
(* let add_pattern (graph : 'a egraph) (pattern : Base.term) (subst : 'var subst) : *)
(*     id = *)
(*   failwith "TODO" *)
(**)
(* let add (graph : 'a egraph) (node : 'a) : id = *)
(*   add_pattern graph node (fun _ -> *)
(*       failwith "The substitution should be empty when adding a new element") *)
