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
    i * 2
}

func main() {
    1 * 2
}
"
    p.run code |> printfn "%i\n"
    let fast = p.getFlatAST()
    fast |> printfn "%A"

    let ti = TypeInference(fast)
    ti.init()
    0
