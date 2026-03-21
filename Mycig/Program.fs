open FParsec
open Mycig.Compiler
open Mycig

[<EntryPoint>]
let main _ =
    let p = Parser()
    let code = @"
package main

import std::fmt

func f(i) {
    i * 2i8
}

func main() {
    1 * 2
}
"
    p.run code |> printfn "%i\n"
    p.getFlatAST() |> printfn "%A"
    
    0
