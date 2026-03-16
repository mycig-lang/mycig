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
            

type CSAParser = string * string list -> PLResult

[<AutoOpen>]
module CSAParserLib =
    let lstr s =
        fun (code, result) ->
            if code.StartsWith (s: string)
            then
                Success(code[s.Length..], result@[s])
            else
                Failure("")
        : CSAParser
    let lrun p code = (p: CSAParser) (code, [])