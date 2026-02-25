namespace Aegith

open System.IO

type ReadFile() =
    static member Read (path: string) =
        if path.EndsWith ".agh"
        then Error ""
        else
            use sr = new StreamReader(path)
            Ok (sr.ReadToEnd())
