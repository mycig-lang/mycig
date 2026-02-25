namespace Aegith.Compiler

type SimpleScope() =
    let mutable scope = [|[||]|]
    let mutable len = 0

    member _.createScope() =
        scope <- scope |> Array.append [|[||]|]
        len <- len + 1

    member _.deleteScope() =
        if
            len = 0
            |> not
        then scope <- scope[..len]

    member _.add (s: string) =
        if
            scope
            |> Array.concat
            |> Array.contains s
            |> not
        then scope[len] <- scope[len] |> Array.append [|s|]
