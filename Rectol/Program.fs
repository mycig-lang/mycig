open FParsec
open Mycig.Compiler
open Mycig.Lexer
open Mycig

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

    let iter = Iterator("abc")
    for _ in 1..4 do
        iter.get() |> printfn "%A"

    0
