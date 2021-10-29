//type exp =  | C of int
//            | BinOp of exp * string * exp;;

// Exercise 1
BinOp(BinOp(C(3), "+", C(5)), "*", C(2))
BinOp(C(1), "+", C(2))
BinOp(C(6), "-", C(10))

// Exercise 2
let rec toString expr =
    match expr with
        | C(value) -> value.ToString()
        | BinOp(exp1, op, exp2) -> "(" + toString(exp1) + op + toString(exp2) + ")"

// Exercise 3
let rec extractOps expr =
    match expr with
        | C(value) -> set []
        | BinOp(exp1, op, exp2) -> Set.union (Set.union (extractOps(exp1)) (set [op])) (extractOps(exp2))

// Exercise 4
//type exp =  | C of int
//            | BinOp of exp * string * exp
//            | Id of string
//            | Def of string * exp * exp;;
let isDef expr =
    let rec extractIds expr =
        match expr with
            | C(_) -> set []
            | BinOp(exp1, _, exp2) -> Set.union (extractIds(exp1)) (extractIds(exp2))
            | Id(id) -> set [id]
            | Def(_, exp1, exp2) -> Set.union (extractIds(exp1)) (extractIds(exp2))
    let rec extractDefs expr =
        match expr with
            | C(_) -> set []
            | BinOp(exp1, _, exp2) -> Set.union (extractDefs(exp1)) (extractDefs(exp2))
            | Id(_) -> set []
            | Def(def, exp1, exp2) -> Set.union (set [def]) (Set.union (extractDefs(exp1)) (extractDefs(exp2)))
    Set.isEmpty (Set.difference (extractIds expr) (extractDefs expr))