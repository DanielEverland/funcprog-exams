// Exercise 1
//let rec f n = function  | 0             -> 1
//                        | k when k > 0  -> n * (f n (k-1))
//                        | _             -> failwith "illegal argument"
//f 2 2;;

let rec g p f =
    function
    | [] -> []
    | x :: xs when p x -> f x :: g p f xs
    | _ :: xs -> g p f xs

g (fun n -> n > 0) (fun n -> n * n) [ -1; 1; 2 ]


type T =
    | A of int
    | B of string
    | C of T * T

let rec h =
    function
    | A n -> string n
    | B s -> s
    | C (t1, t2) -> h t1 + h t2

h (C(B("ben"), A(10)))


// Exercise 2
// f: int -> int -> int
// g: 'a -> bool -> 'a -> 'b -> 'a list -> 'b list
// h: T -> string

// Exercise 3
let rec f n =
    function
    | 0 -> 1
    | k when k > 0 -> n * (f n (k - 1))
    | _ -> failwith "illegal argument"
f 2 5

let rec f2 c n =
    match n with
    | 1 -> c
    | n -> f2 (c * 2) (n - 1)
f2 2 2

let rec f3 n c k =
    match c with
    | 0             -> k 1
    | c when c > 0  -> f3 n (c - 1) (fun v -> k(n * v))
    | _             -> failwith "illegal argument"
f3 2 5 id