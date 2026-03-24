namespace Mycig.Compiler

open System.Text.RegularExpressions

type ASTInserter(fast: FlatAST) =
    let refRegex = Regex(@"ref:\s*(-?\d+)", RegexOptions.Compiled)
    let mutable node = fast.getAST()

    let shiftRefData insertIndex delta (data: string) =
        refRegex.Replace(
            data,
            MatchEvaluator(fun m ->
                let value = int m.Groups[1].Value

                if value >= insertIndex then
                    sprintf "ref: %i" (value + delta)
                else
                    m.Value)
        )

    let shiftRefs insertIndex delta =
        node <-
            node
            |> Array.map (fun ast ->
                { ast with
                    Data = shiftRefData insertIndex delta ast.Data })

    member __.insert(index: int) (ast: ASTNode) =
        __.insertMany index [ ast ]

    member __.insertMany(index: int) (asts: ASTNode list) =
        match asts with
        | [] -> ()
        | _ ->
            shiftRefs index asts.Length
            node <- node |> Array.insertManyAt index (asts |> List.toArray)

    member __.replace(index: int) (ast: ASTNode) =
        node[index] <-
            { Type = ast.Type
              Line = ast.Line
              Column = ast.Column
              Data = ast.Data }

    member __.commit() =
        fast.setAST node
        fast.initData()

    member __.getAST() = node
