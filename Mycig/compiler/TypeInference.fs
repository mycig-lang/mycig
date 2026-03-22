namespace Mycig.Compiler

type TypeInference(fast: FlatAST) =
    let flatAST = fast.getAST() |> Array.indexed
    let mutable frames = [||]

    member this.init() =
        frames <- flatAST |> Array.filter (fun (_, x) -> x.Type = "frame")
