open Base
open Critical
open Orders
open Rewriting

module type TermSet = Set.S with type elt = term

module Basic = struct
  let rec complete (order : term order) (trs : (term * term) list) =
    let rules =
      critical_pairs trs
      |> List.map (fun (l, r) ->
             let l, r = (norm trs l, norm trs r) in
             match order (l, r) with
             | Some comp -> (
                 match comp with
                 | GT -> (l, r)
                 | LT -> (r, l)
                 | EQ -> failwith "Tried to reduce a non critical pair")
             | None -> failwith "incomparable critical pair")
    in
    let trs = TermSet.of_list trs in
    let rules = TermSet.union trs rules in
    if rules = trs then rules else complete order trs

  let complete (order : term order) (identities : (term * term) list) = ()
end
