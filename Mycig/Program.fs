open Mycig.Compiler

[<EntryPoint>]
let main _ =
    let p = Parser()
    let code = @"
package main

import std::fmt

func main() {
    if 1 {
        2
    }
    else {
        3
    }
}
"
    p.run code |> printfn "%i\n"
    p.getFlatAST() |> printfn "%A"
    0