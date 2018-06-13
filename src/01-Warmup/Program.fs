// Learn more about F# at http://fsharp.org

open System

type Id = string

type BinOp = Plus | Minus | Times | Div

type Stm =
    | CompoundStm of Stm * Stm
    | AssignStm of Id * Expr
    | PrintStm of Expr list
and Expr =
    | IdExpr of Id
    | NumExpr of int
    | OpExpr of Expr * BinOp * Expr
    | ExprSeqExpr of Stm * Expr

let prog =
    CompoundStm (AssignStm ("a", OpExpr (NumExpr 5, Plus, NumExpr 3)),
        CompoundStm 
            (AssignStm ("b",
                ExprSeqExpr (PrintStm [IdExpr "a"; OpExpr (IdExpr "a", Minus, NumExpr 1)],
                    OpExpr (NumExpr 10, Times, IdExpr "a"))),
            PrintStm [IdExpr "b"]))

let maxArgs = 
    let rec maxArgsStm = function
        | CompoundStm (s, t) -> max (maxArgsStm s) (maxArgsStm t)
        | AssignStm (_, s) -> maxArgsExpr s
        | PrintStm es -> es |> List.map maxArgsExpr |> List.fold max (List.length es)
    and maxArgsExpr = function
        | IdExpr _ -> 0
        | NumExpr _ -> 0
        | OpExpr (e, _, f) -> max (maxArgsExpr e) (maxArgsExpr f)
        | ExprSeqExpr (s, e) -> max (maxArgsStm s) (maxArgsExpr e)

    maxArgsStm

let interp =
    let opFun = function
        | Plus -> (+)
        | Minus -> (-)
        | Times -> (*) (* un-confuse the syntax highlighting *)
        | Div -> (/)

    let rec interp env = function
        | CompoundStm (s, t) -> let env' = interp env s in interp env' t
        | AssignStm (id, e) -> (id, eval env e |> fst) :: env
        | PrintStm es -> printfn "%s" (es |> List.map (eval env >> fst >> sprintf "%d") |> String.concat " "); env
    and eval env = function
        | IdExpr id ->
            env 
            |> List.tryPick (fun (id', v) -> if id = id' then Some v else None)
            |> function Some v -> (v, env) 
                      | None -> failwithf "Unknown identifier: %s" id
        | NumExpr v -> (v, env)
        | OpExpr (v, op, w) -> 
            let (v', env') = eval env v
            let (w', env'') = eval env' w
            (opFun op v' w', env'')
        | ExprSeqExpr (s, e) -> let env' = interp env s in eval env' e

    interp [] >> ignore

[<EntryPoint>]
let main argv =
    printfn "Max args: %d" (maxArgs prog)

    interp prog
    0 // return an integer exit code
