namespace Mycig.ConcreteSyntaxAnalysis

[<StructuredFormatDisplay("{DisplayString}")>]
type PLResult =
    | Success of string * string list
    | Failure of string
    
    member this.DisplayString =
        match this with
        | Success(_, result) ->
            sprintf "Success!: %A" result
        | Failure(err) ->
            sprintf "Failure!: %s" err
            

type CSAParser = PLResult -> PLResult

[<AutoOpen>]
module CSAParserLib =
    let lstr s =
        function
        | Success(code, result) ->
            if code.StartsWith (s: string)
            then
                Success(code[s.Length..], result@[s])
            else
                Failure("")
        | plresult -> plresult
        : CSAParser

    let lrun p code = (p: CSAParser) (Success(code, []))

    let (>*) p1 p2 =
        (p1 : CSAParser)
        >> function
            | Success(code, result) -> Success(code.TrimStart ' ', result)
            | plresult -> plresult
        >> p2
        : CSAParser

    let (>+) p1 p2 =
        (p1 : CSAParser)
        >> function
            | Success(code, result) ->
                if code[0] = ' '
                then Success(code.TrimStart ' ', result)
                else Failure("")
            | plresult -> plresult
        >> p2
        : CSAParser