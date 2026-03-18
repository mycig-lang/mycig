open FParsec
open Mycig.Compiler
open Mycig.ConcreteSyntaxAnalysis
open Mycig

//    let p = Parser()
//    let code = @"
//package main

//import std::fmt

//func f(i) {
//    1 * 2
//}

//func main() {
//    1 * 2
//}
//"
//    p.run code |> printfn "%i\n"
//    p.getFlatAST() |> printfn "%A"
    //let abc = lstr "abc" >+ lstr "def"
    //lrun abc "abcdef" |> printfn "%A"
    
    //0

type Expression =
    | Normal
    | Laugh
    | Angry
    | Smile

type Script =
    { Speaker : string; Text : string ; Expression : Expression}

type ScriptBuilder() =
    member __.Yield(_) =
        { Speaker="" ; Text="" ; Expression=Normal}

    [<CustomOperation("speaker")>]
    member __.SetSpeaker(script, speaker) =
        {script with Speaker=speaker}

    [<CustomOperation("text")>]
    member __.SetText(script, text) =
        {script with Text=text}

    [<CustomOperation("expression")>]
    member __.SetExpression(script, expression) =
        {script with Expression=expression}

let script = ScriptBuilder()
let page1 = script {
    speaker "Nick"
    text "Hello, Aki."
    expression Smile
}

printfn "%A" page1