open Mycig.CutParser

[<EntryPoint>]
let main _ =
    let p = Parser().string("a").string("s")
    p.buf |> printfn "%A"
    p.run "as" |> printfn "%A"
    0