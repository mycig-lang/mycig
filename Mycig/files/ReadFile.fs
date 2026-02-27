namespace Mycig

open System.IO

type ReadFile() =
    static member Read (path: string) =
        if path.EndsWith ".mg"
        then Error ""
        else
            use sr = new StreamReader(path)
            Ok (sr.ReadToEnd())
