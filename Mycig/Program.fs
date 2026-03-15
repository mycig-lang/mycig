open Mycig.Compiler

[<EntryPoint>]
let main _ =
    let p = Parser()
    let code = @"
package main

import std::fmt

func f(i) {
    1 * 2
}

func main() {
    1 * 2
}
"
    p.run code |> printfn "%i\n"
    p.getFlatAST() |> printfn "%A"
    0