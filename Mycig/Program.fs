open FParsec
open Mycig.Compiler
open Mycig.VerificationScript
open Mycig

[<EntryPoint>]
let main _ =
    let p = Parser()
    let code = @"
package main

import std::fmt

func f(i) {
    1 * 2
}

func main() {
    1 * 2
}
"
    p.run code |> printfn "%i\n"
    p.getFlatAST() |> printfn "%A"
    let code = """
    states { Fresh, Used, Purged }

rule Use { Fresh -> Used }

infer MemoryError : Unit {
    Use(Fresh, Used);
    Purge(Used, Purged);
}
"""
    let rf = ReadFile("mgvs")
    let re = rf.Read("script/mycig.mgvs")
    let code = if re.IsOk then re.ToString() else ""
    match run VSParser.pProgram code with
    | Success(res, _, _) ->
        VSEngine.runAnalysis res
    | Failure(err, _, _) ->
        printfn "Parse Error:\n%s" err
    0