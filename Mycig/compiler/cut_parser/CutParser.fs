namespace Mycig.CutParser

type Child<'T, 'V> = {
    main: 'T -> 'V
    validate: 'T -> bool
}

type Parser() =
    let mutable buf = [||]

    member this.validate f =

