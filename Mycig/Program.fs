open FParsec
open Mycig.Compiler
open Mycig.Lexer
open Mycig

[<EntryPoint>]
let main _ =
//    let p = Parser()

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

    let lexer = Lexer(0, "abc")
    0
