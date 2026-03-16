open FParsec
open Mycig.Compiler
open Mycig.ConcreteSyntaxAnalysis
open Mycig

[<EntryPoint>]
let main _ =
//    let p = Parser()
//    let code = @"
//package main

//import std::fmt

//func f(i) {
//    1 * 2
//}

//func main() {
//    1 * 2
//}
//"
//    p.run code |> printfn "%i\n"
//    p.getFlatAST() |> printfn "%A"
    let abc = lstr "abc" -- lstr "def"
    lrun abc "abc" |> printfn "%A"
    0