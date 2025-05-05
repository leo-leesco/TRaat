open Base
open Critical
open Orders

module Basic = struct
  let rec complete (identities : (term * term) list) (order : (term order) option)
      ?(trs :  Set.(term * term) = Set.empty) =
    let rules = critical_pairs trs |> List.map (fun (l,r) ->
        let (l,r) = (norm trs l, norm trs r) in
          match order l r with
          | Some comp -> (match comp with
          | GT -> (l,r)
                  | LT -> (r,l) | EQ -> failwith "Tried to reduce a non critical pair") | None -> failwith "incomparable critical pair"
      ) in
end
