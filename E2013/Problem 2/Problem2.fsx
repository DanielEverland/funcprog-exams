let rec f i =
    function
    | [] -> []
    | x::xs -> (i, x)::f (i*i) xs
f 10 [1; 2; 3; 4]

// Exercise 2
// 1
let rec f2 i acc xs =
    match xs with
    | [] -> List.rev([]@acc)
    | x::xs -> f2 (i*i) ((i, x)::acc) xs
f2 10 [] [1; 2; 3; 4]

// 2
let rec f3 i k xs =
    match xs with
    | [] -> k []
    | x::xs -> f3 (i*i) (fun v -> k((i, x)::v)) xs
f3 10 id [1; 2; 3; 4]