namespace Mycig.CutParser

//type ParserForOption<'T> =
//    | PNone
//    | PSome of 'T

//type poption<'T> = ParserForOption<'T>

type Child<'T, 'V> = {
    Main: 'T -> 'V
    mutable Validate: 'T -> bool
}

exception ParseError of string

type Parser() =
    member val buf: Child<string, obj * string> array = [||] with get, set
    member val res: obj array = [||] with get, set

    member this.validate f =
        this.buf[this.buf.Length - 1].Validate <- f
        this

    member this.string (s: string) =
        let elt: Child<string, obj * string> =
            {
                Main = fun ctx -> (s, ctx[s.Length..])
                Validate = fun ctx -> ctx.StartsWith s
            }
        this.buf <- [|elt|] |> Array.append this.buf
        this

    member this.run str =
        let mutable str = str
        for item in this.buf do
            if item.Validate str then
                let (s, ctx) = item.Main str
                this.res <- this.res |> Array.append [|s|]
                str <- ctx
            else raise (ParseError(""))
        str
