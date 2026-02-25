namespace Aegith.Compiler

type TypeInference(fast: FlatAST) =
    member val private FlatAST = fast.getAST() |> Array.indexed with get, set
    member val private Structs = [||] with get, set
    member val private Protocols = [||] with get, set
    member val private Funcs = [||] with get, set
    member val private FuncSTs = [||] with get, set
    member val private FuncPRs = [||] with get, set

    member this.init() =
        this.Structs <- this.FlatAST |> Array.filter (fun (_, x) -> x.Type = "struct")
        this.Protocols <- this.FlatAST |> Array.filter (fun (_, x) -> x.Type = "protocol")
        this.Funcs <- this.FlatAST |> Array.filter (fun (_, x) -> x.Type = "func")
        this.FuncSTs <- this.FlatAST |> Array.filter (fun (_, x) -> x.Type = "func_st")
        this.FuncPRs <- this.FlatAST |> Array.filter (fun (_, x) -> x.Type = "func_pr")
