open FParsec
open Mycig.Compiler
open Mycig

[<EntryPoint>]
let main _ =
    let p = pint32
    run p "-1" |> printfn "%A"
    
    let p = Parser()
    let code = @"
package main

import std::fmt

func main() {
    let a = 1i8
    a
}
"
    p.run code |> printfn "%i\n"
    let fast = p.getFlatAST()
    fast |> printfn "%A"

    let ti = TypeInference(fast)
    ti.init()
    ti.inferNode(3) |> printfn "%A"
    
    0
