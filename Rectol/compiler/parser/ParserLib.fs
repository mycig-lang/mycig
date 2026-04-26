namespace Rectol.Compiler.Library

type Parser<'a> = Parser of (string -> option<'a * string>)

[<AutoOpen>]
module PabulicParserFunc =
    let run (Parser p) input = p input

    let fail = Parser (fun _ -> None)

module private ParserFunc =
    let result x = Parser (fun input -> Some (x, input))

    let bind f p =
        Parser (fun input ->
            match run p input with
            | Some (v, rest) -> run (f v) rest
            | None -> None)

type ParserBuilder() =
    member _.Bind(p, f) = ParserFunc.bind f p
    member _.Return(x) = ParserFunc.result x
    member _.ReturnFrom(p) = p
    member _.Zero() = fail

[<AutoOpen>]
module ComputationExpressionForParser =
    open ParserFunc
    open System

    let parser = ParserBuilder()
    
    let (<|>) p1 p2 =
        Parser (fun input ->
            match run p1 input with
            | Some _ as res -> res
            | None -> run p2 input)

    let skip1 =
        Parser (fun input ->
            if input = "" then None
            else Some (input[0], input[1..]))

    let skip i =
        Parser (fun input ->
            if input = "" then None
            else Some (input[0..i-1], input[i..]))

    let satisfy1 pred =
        parser {
            let! c = skip1
            if pred c then return c
            else return! fail
        }

    let satisfy pred i =
        parser {
            let! c = skip i
            if pred c then return c
            else return! fail
        }

    let pchar c = satisfy1 ((=) c)

    let pstring (s: string) =
        Parser (fun input ->
            if input.StartsWith(s) then
                Some (s, input.Substring(s.Length))
            else None)

    let rec many p =
        (parser {
            let! x = p
            let! xs = many p
            return x :: xs
        }) <|> result []

    let many1 p =
        parser {
            let! x = p
            let! xs = many p
            return x :: xs
        }

    let pdigit = satisfy1 Char.IsDigit

    let pnumber =
        parser {
            let! digits = many1 pdigit
            return digits |> List.toArray |> String |> int
        }
