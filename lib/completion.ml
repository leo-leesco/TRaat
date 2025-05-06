open Base
open Critical
open Orders
open Rewriting

module TermSet = Set.Make (struct
  type t = term * term

  let compare = Stdlib.compare
end)

module Basic = struct
  let rec complete (order : term partial_order) (trs : TermSet.t) =
    let rules =
      critical_pairs (TermSet.to_list trs)
      |> List.map (fun (l, r) ->
             let l, r = (norm trs l, norm trs r) in
             match order l r with
             | Some comp ->
                 if comp > 0 then (l, r)
                 else if comp < 0 then (r, l)
                 else failwith "Tried to reduce a non critical pair"
             | None -> failwith "incomparable critical pair")
      |> TermSet.of_list
    in
    let rules = TermSet.union trs rules in
    if rules = trs then rules else complete order trs

  let complete (order : term partial_order) (identities : (term * term) list) =
    let trs =
      List.filter_map
        (fun (l, r) ->
          match order l r with
          | Some sg when sg > 0 -> Some (l, r)
          | Some sg when sg < 0 -> Some (r, l)
          | _ -> None)
        identities
      |> TermSet.of_list
    in
    if TermSet.is_empty trs then failwith "no rule can be reordered"
    else complete order trs |> TermSet.to_list
end
