namespace Mycig.Lexer

type Iterator(str: string) =
    new() = Iterator("")

type Lexer(_a: obj, str: string) =
    let parseStr = Iterator(str)
    let memo = Iterator()

    member __.get() =
        parseStr
