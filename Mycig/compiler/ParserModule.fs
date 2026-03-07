namespace Mycig.Compiler

open FParsec

[<AutoOpen>]
module ParserModule =
    let inline achoice p lst =
        if lst |> List.length = 0
        then p
        else
            (lst |> List.map attempt) @ [p] |> choice