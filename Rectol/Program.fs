open Psictre
open Rectol.Lexer
open Rectol

// =====================
// AST
// =====================

type Expr =
    | Int of int
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Pow of Expr * Expr


// =====================
// トークン処理
// =====================

let whitespace =
    many (satisfy1 System.Char.IsWhiteSpace)

let token p =
    parse {
        let! v, _ = p
        let! _ = whitespace
        return v
    }

let symbol s = token (pstring s)


// =====================
// 再帰パーサ
// =====================

let rec expr input = exprImpl input
and term input = termImpl input
and power input = powerImpl input
and factor input = factorImpl input


// =====================
// factor
// =====================

and factorImpl input =
    (parse {
        let! n, _ = token pint32
        return Int n
    })
    <|>
    (parse {
        let! _, input = symbol "("
        let! e, _ = expr input
        let! _ = symbol ")"
        return e
    })


// =====================
// power（右結合）
// =====================

and powerImpl input =
    parse {
        let! baseExpr, _ = factor input
        return!
            (parse {
                let! _, input = symbol "^"
                let! exponent, _ = power input
                return Pow(baseExpr, exponent)
            })
            <|> (parse {
                return baseExpr
            })
    }


// =====================
// term（* /）
// =====================

and termImpl input =
    let rec loop acc =
        (parse {
            let! _, input = symbol "*"
            let! rhs, _ = power input
            return! loop (Mul(acc, rhs))
        })
        <|>
        (parse {
            let! _, input = symbol "/"
            let! rhs, _ = power input
            return! loop (Div(acc, rhs))
        })
        <|> (parse {
            return acc
        })

    parse {
        let! first, input = power input
        return! loop first
    }


// =====================
// expr（+ -）
// =====================

and exprImpl input =
    let rec loop acc =
        (parse {
            let! _, input = symbol "+"
            let! rhs, _ = term input
            return! loop (Add(acc, rhs))
        })
        <|>
        (parse {
            let! _, input = symbol "-"
            let! rhs, _ = term input
            return! loop (Sub(acc, rhs))
        })
        <|> (parse{
            return acc
        })

    parse {
        let! first, _ = term input
        return! loop first
    }


// =====================
// エントリ
// =====================

let exprParse input =
    match run (parse {
        let! _ = whitespace
        let! e, _ = expr input
        let! _ = eof
        return e
    }) input
    with
    | Ok (ast, _) -> Ok ast
    | Error e -> Error e

[<EntryPoint>]
let main _ =
//    let p = Parser()
// <T1: [U1, U2], T2: [U3, U4, U5]>
//    let code =
//        @"
//package main

//import std::fmt

//func f() -> &mut i32 {
//    100
//}

//func main() {
//    let a = f()
//    let b = &mut a
//}
//"

//    p.run code |> printfn "%i\n"
//    let fast = p.getFlatAST()

//    printfn "%A" fast

    //exprParse "1@2^2^3" |> printfn "%A"
    //exprParse "1+2+3" |> printfn "%A"

    let p = parse {
        let! _ = pstring "a"
        return ()
    }
    run p "b" |> printfn "%A"

    0
