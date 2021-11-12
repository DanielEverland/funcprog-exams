let rec g1 p =
    function
    | x :: xs when p x  -> x :: g1 p xs
    | _                 -> []
g1 (fun j -> j < 2) [-1; 0; 1; 2]

// Exercise 1
// ('a -> bool) -> 'a list -> 'a list

let rec g2 f h n x = 
    match n with
    | _ when n < 0  -> failwith "negative n is not allowed"
    | 0             -> x
    | n             -> g2 h f (n-1) (f x)

// (int -> int -> 'a ) (int -> int -> int -> int) -> int -> 'a

// Exercise 2
let rec g3 p lst k =
    match lst with
    | x::xs when p x -> g3 p xs (fun v -> k(x::v)) 
    | _ -> k []
g3 (fun j -> j < 2) [-1; 0; 1; 2] id