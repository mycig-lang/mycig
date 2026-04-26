open Rectol.Compiler.Library
open Rectol.Lexer
open Rectol

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
    
    let p = parser {
        let! list = many1 (pstring "a")
        printfn "%A" list
        return list |> String.concat ""
    }

    run p "aaabb" |> printfn "%A"

    0
