namespace Mycig

open System.IO

type ReadFile(extension) =
    member _.Read (path: string) =
        if path.EndsWith (sprintf ".%s" extension)
        then Error ""
        else
            use sr = new StreamReader(path)
            Ok (sr.ReadToEnd())
