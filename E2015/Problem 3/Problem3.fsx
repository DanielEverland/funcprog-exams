type Name       = string
type Flow       = int
type River      = R of Name * Flow * Tributaries
and Tributaries = River List

// Exercise 1
let riv3 = R("R3", 2, [])
let riv = R("R", 10, [R("R1", 5, []); R("R2", 15, [R("R4", 2, [])]); riv3])

// Exercise 2
let rec contains name river =
    match river with
        | R(rName, _, _) when rName = name -> true
        | R(_, _, tributaries) -> (List.fold (fun acc elem -> acc + (if contains name elem then 1 else 0)) 0 tributaries) > 0
        | _ -> false

contains "R" riv;;
contains "R1" riv;;
contains "R2" riv;;
contains "R3" riv;;
contains "R4" riv;;