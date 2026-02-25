namespace Aegith.Compiler

open FParsec

type ASTNode = {
    Type: string;
    Line: int64
    Column: int64
    mutable Data: string
}

exception FASTParseException of string

type FASTResult<'T, 'E> =
    | Ok of 'T
    | Err of 'E
    member this.unwrap() =
        match this with
        | Ok v -> v
        | Err _ -> failwith "FASTResult.unwrap() ->! It's Not FASTRsult.Ok."

type FlatAST() =
    let ident =
        regex @"[\p{L}_][\p{L}\p{N}_]*"
    let bool_ =
        pstring "bool"
        .>> spaces
        .>> pchar ':'
        .>> spaces
        >>. choice [
            attempt (stringReturn "true" true)
            stringReturn "false" false
        ]
        .>> spaces
        |>> box
    let str_ =
        pstring "str"
        .>> spaces
        .>> pchar ':'
        >>. between
            (spaces .>> pchar '"')
            (pchar '"' .>> spaces)
            ident
        .>> spaces
        |>> box
    let ref_ =
        pstring "ref"
        .>> spaces
        .>> pchar ':'
        .>> spaces
        >>. pint32
        .>> spaces
        |>> box
    let arr_, arrRef = createParserForwardedToRef()
    let program =
        between
            (spaces .>> pchar '[' .>> spaces)
            (pchar ']' .>> spaces)
            (sepBy
                (choice [
                    bool_
                    str_
                    ref_
                    arr_
                ])
                (spaces .>> pchar ',' .>> spaces)
            )
        .>> eof

    do
        arrRef.Value <-
            pstring "arr"
            .>> spaces
            .>> pchar ':'
            >>. between
                (spaces .>> pchar '[' .>> spaces)
                (spaces .>> pchar ']' .>> spaces)
                (sepBy
                    (choice [
                        bool_
                        str_
                        ref_
                        arr_
                    ])
                    (pchar ',' .>> spaces)
                )
            .>> spaces
            |>> box

    member val private AST = [||] with get, set
    member val private Data = [||] with get, set

    override this.ToString (): string = 
        sprintf "FlatAST:\n%s\n\n" (this.AST |> Array.mapi (sprintf "[ %i ]: %A\n") |> String.concat "\n")

    member this.add(ast) =
        this.AST <- [|ast|] |> Array.append this.AST
        this.AST.Length - 1

    member this.initData() =
        this.Data <-
            this.AST
            |> Array.map
                (fun ast ->
                    match run program ast.Data with
                    | Success(res, _, _) -> res
                    | Failure(msg, _, _) -> failwith <| sprintf "FlatAST.Data ->! Failured Parse.\n%s" msg
                )

    member this.getAST() =
        this.AST

    member this.getAST i =
        if i < 0 || this.AST.Length <= i
        then Err("FlatAST.Ast ->! Index Out Of Range.")
        else Ok(this.AST[i])

    member this.getData() =
        this.Data

    member this.getData i =
        if i < 0 || this.Data.Length <= i
        then Err("FlatAST.Data ->! Index Out Of Range.")
        else Ok(this.Data[i])
    // TODO
