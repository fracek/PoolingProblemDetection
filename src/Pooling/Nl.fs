module Pooling.Nl

type Expr =
    | Negate of Expr
    | Product of Expr list
    | Sum of Expr list
    | Minus of Expr list
    | Divide of Expr * Expr

    | Exp of Expr
    | Power of Expr * Expr
    | Ln of Expr

    | Var of double * int (* coef * idx *)
    | Num of double

let rec eval (vars : double list) ex : double =
    let ev = eval vars
    match ex with
    | Negate e -> - (ev e)
    | Product es -> es |> List.map ev |> List.reduce (*)
    | Sum es -> es |> List.map ev |> List.reduce (+)
    | Minus es -> es |> List.map ev |> List.reduce (-)
    | Divide (e1, e2) -> (ev e1) / (ev e2)
    | Exp e -> exp(ev e)
    | Power (e1, e2) -> (ev e1) ** (ev e2)
    | Ln e -> log(ev e)
    | Var (coef, idx) -> coef * vars.[idx]
    | Num n -> n
