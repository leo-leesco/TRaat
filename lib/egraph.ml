(** represents equivalence classes as connected components of a graph *)
module UnionFind : sig
  type id = int
  (** should be a valid index i.e. be lower than the number of elements in the
      set *)

  val string_of_idx : id -> string

  type 'a classes

  val length : 'a classes -> int

  val map : (id -> 'a -> 'b) -> 'a classes -> 'b list
  (** not a pure [map], the [id] taken is the parent *)

  val ( .!() ) : 'a classes -> id -> 'a
  (** access the data contained inside a node *)

  val create : unit -> 'a classes
  val make : 'a list -> 'a classes

  val add : 'a classes -> 'a -> id
  (** modifies the graph in-place and returns its new [id] *)

  val find : 'a classes -> id -> id
  val union : 'a classes -> id -> id -> id

  val join : ?f:(id -> 'a -> 'a) -> 'a classes -> 'a classes -> 'a classes
  (** merge two unconnected sets together and applies [f] to the second one

      equivalence classes are preserved

      [f] first argument is the offset, in case [data] relies on [id] *)
end = struct
  type id = int

  let string_of_idx (idx : id) = string_of_int idx

  type 'a node = { data : 'a; mutable parent : id; mutable depth : id }
  (** [depth] should be the max number of children, 0 when a leaf

      [parent] is the index in the array

      (not reduced by path compression, so it is actually worst case) *)

  type 'a classes = 'a node Dynarray.t

  (* those serve to mimic the interface of [Array] *)
  let ( .!() ) = Dynarray.get
  (* let ( .!()<- ) = Dynarray.set *)

  let length graph = Dynarray.length graph
  let create = Dynarray.create

  let map f graph =
    Dynarray.to_list (Dynarray.map (fun node -> f node.parent node.data) graph)

  let make data =
    Dynarray.mapi
      (fun idx data -> { data; parent = idx; depth = 0 })
      (Dynarray.of_list data)

  let add (graph : 'a classes) data =
    let id = Dynarray.length graph in
    Dynarray.add_last graph { data; parent = id; depth = 0 };
    id

  let assert_valid_id (graph : 'a classes) (node_idx : id) =
    if not (0 <= node_idx && node_idx <= Dynarray.length graph) then
      raise (Invalid_argument "Tried to access an out-of-bounds node")

  (** returns the index of the ancestor

      performs path compression *)
  let rec find (graph : 'a classes) node_idx =
    assert_valid_id graph node_idx;

    if graph.!(node_idx).parent <> node_idx then (
      graph.!(node_idx).parent <- find graph node_idx;
      graph.!(node_idx).parent)
    else node_idx

  let union graph n m =
    let n = find graph n in
    let m = find graph m in

    if graph.!(n).depth > graph.!(m).depth then (
      graph.!(m).parent <- n;
      n)
    else if graph.!(n).depth < graph.!(m).depth then (
      graph.!(n).parent <- m;
      m)
    else (
      graph.!(m).parent <- n;
      graph.!(n).depth <- graph.!(n).depth + 1;
      n)

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

open Base

type id = UnionFind.id
type 'a enode = { data : 'a; children : id list }
type 'a classes = 'a enode UnionFind.classes

type 'a egraph = { unionfind : 'a classes; hashcons : ('a enode, id) Hashtbl.t }
(** an [egraph] is represented by the UnionFind classes that can be queried
    according to their [id], and reciprocally you can get the [id] of a class
    from a [enode], which is useful when you have to check whether a [term] is
    represented in the [egraph]*)

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

let empty : 'a egraph =
  { unionfind = UnionFind.create (); hashcons = Hashtbl.create 0 }

(** retrieves the [id] of the [term] looked up, and creates it in case it is not
    yet stored in the [egraph] *)
let rec of_term ?(eg : symbol egraph = empty) (expr : term) : id =
  let enode =
    match expr with
    | V x -> { data = Var x; children = [] }
    | T (f, t) -> { data = F f; children = List.map (of_term ~eg) t }
  in

  (* if [expr] is already represented in [eg], no modification is done to [eg]
   * otherwise, we create a new node *)
  try Hashtbl.find eg.hashcons enode
  with Not_found ->
    let add (eg : 'a egraph) (node : 'a enode) : id =
      let new_id = UnionFind.add eg.unionfind node in
      Hashtbl.add eg.hashcons node new_id;
      new_id
    in

    add eg enode

(* those functions are actually aliases for [of_term], because that allows for reuse of exactly the same logic *)
let find (eg : symbol egraph) (expr : term) : id = of_term ~eg expr
let add_term (eg : symbol egraph) (expr : term) : id = of_term ~eg expr

let union (eg : 'a egraph) (class1 : id) (class2 : id) : id =
  UnionFind.union eg.unionfind class1 class2

(** extracts a term that only contains representatives

    starts with the given eclass *)
let to_term (eg : symbol egraph) (base : id) : term = failwith "TODO"

(** implements extraction based on cost function

    if the cost function is not provided, simply find the lightest equivalent
    term (each edge gets weigthed equally) *)
let extract_best ?(weight = None) (eg : symbol egraph) : term = failwith "TODO"

(** apply in place the rewrite to the egraph *)
let ( @@= ) rewrite (eg : symbol egraph) = failwith "TODO"
