open Mycig.Compiler

[<EntryPoint>]
let main _ =
    let p = Parser()
    let code = @"
package main

import std::fmt

frame Animal {
    field Self {
        *name: str
    }
    impl Self {
        pub func move(self) {
            1
        }
    }
}
"
    p.run code |> printfn "%i\n"
    p.getFlatAST() |> printfn "%A"
    0