namespace Aegith.Compiler

open FParsec

type Node = string * int64 * int64 * string

type Assoc = Associativity


/// <summary>
/// require type inference: -1
/// </summary>
type Parser() =
    let fast = FlatAST()
    let ss = SimpleScope()

    let endLines = many (newline <|> pchar ';')
    let funcEndLines = many newline
    let stEndLines = many newline
    let prEndLines = many newline
    let ident = regex @"[\p{L}_][\p{L}\p{N}_]*"
    
    let funcTerm, funcTermRef = createParserForwardedToRef()
    let structTerm, structTermRef = createParserForwardedToRef()
    let protocolTerm, protocolTermRef = createParserForwardedToRef()
    let typ, typRef = createParserForwardedToRef()
    let exprTerm, exprTermref = createParserForwardedToRef()
    
    let typp =
        choice [
            pipe2
                getPosition
                (pstring "chan" .>> spaces1 >>. typ)
                (fun pos t ->
                    fast.add {
                        Type = "type_chan"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[ref: %i]" t
                    }
                )
            pipe2
                getPosition
                (
                    pstring "func"
                    >>. between
                        (spaces .>> pchar '(')
                        (spaces .>> pchar ')')
                        (sepBy (spaces >>. typ) (spaces .>> pchar ','))
                    .>>. opt (attempt (spaces >>. typ .>> spaces))
                )
                (fun pos (arg, rettyp) ->
                    fast.add {
                        Type = "type_func"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf
                            "[arr: [%s], ref: %i]"
                            (arg |> List.map (sprintf "ref: %i") |> String.concat ", ")
                            (match rettyp with | Some t -> t | None -> -1)
                    }
                )
            pipe2
                getPosition
                typ
                (fun pos t ->
                    fast.add {
                        Type = "type"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[ref: %i]" t
                    }
                )
        ]

    let block p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (many p)
    let block1 p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (many1 p)
    let blockOrExp p =
        achoice (spaces >>. p |>> (fun x -> [x])) [
            block1 funcTerm
        ]

    let variable = ident

    let opp = OperatorPrecedenceParser()
    
    let package_ =
        pipe2
            getPosition
            (between
                (spaces .>> pstring "package" .>> spaces1)
                endLines
                (sepBy1 ident (attempt (spaces .>> pstring "::" .>> spaces)))
            )
            (fun pos lst ->
                fast.add {
                    Type = "package"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf "[str: \"%s\"]" (lst |> String.concat "::")
                }
            )
        .>> spaces
    let import_ =
        pipe2
            getPosition
            (between
                (spaces .>> pstring "import" .>> spaces1)
                endLines
                (sepBy1 ident (attempt (spaces .>> pstring "::" .>> spaces)))
            )
            (fun pos lst ->
                fast.add {
                    Type = "import"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\"]"
                        (lst |> String.concat "::")
                }
            )
        .>> spaces

    let let_ =
        pipe2
            getPosition
            (pstring "let"
                .>> spaces1
                >>. choice [
                    stringReturn "mut" true
                    pstring "" >>% false
                ]
                .>> spaces
                .>>. choice [
                    stringReturn "*" true .>> spaces
                    pstring "" >>% false
                ]
                .>>. ident
                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
                .>> endLines
            )
            (fun pos ((((ismut, isrepo), name), t), content) ->
                fast.add {
                    Type = "let"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, bool: %b, str: \"%s\", ref: %i, arr: [%s]]"
                        ismut
                        isrepo
                        name
                        (
                            match t with
                            | Some t -> t
                            | None -> -1
                        )
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let val_st =
        pipe2
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)) .>> pstring "val" .>> spaces1
                .>>. ident
                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
                .>> endLines
            )
            (fun pos (((isPub, name), t), content) ->
                fast.add {
                    Type = "val_struct"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s]]"
                        (match isPub with | Some v -> v | None -> false)
                        name
                        (
                            match t with
                            | Some t -> t
                            | None -> -1
                        )
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let val_pr =
        pipe2
            getPosition
            (opt (attempt (stringReturn "abs" true .>> spaces1)) .>> pstring "val" .>> spaces1
                .>>. ident
                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
            )
            (fun pos (((isAbs, name), t), content) ->
                fast.add {
                    Type = "val_protocol"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s]]"
                        (match isAbs with | Some v -> v | None -> false)
                        name
                        (
                            match t with
                            | Some t -> t
                            | None -> -1
                        )
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )

    let func_ =
        pipe2
            getPosition
            (opt (stringReturn "pub" true .>> spaces1) .>> pstring "func" .>> spaces1
                .>>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (sepBy
                        (
                            spaces
                            >>. opt (attempt (stringReturn "*" 0uy))
                            .>>. ident
                            .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                        )
                        (spaces .>> pchar ',')
                    )
                .>>. opt (attempt (spaces >>. typp .>> spaces))
                .>>. block1 funcTerm
                .>> funcEndLines
            )
            (fun pos ((((isMod, name), args), rettyp), content) ->
                fast.add {
                    Type = "func"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s], arr[%s]]"
                        (match isMod with | Some v -> v | None -> false)
                        name
                        (match rettyp with | Some typ -> typ | None -> -1)
                        (args |> List.map (fun ((isrepo, f), s) -> sprintf "arr: [bool: %b, str: \"%s\", ref: %i]" (match isrepo with | Some _ -> true | None -> false) f (match s with | Some i -> i | None -> -1)) |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String .concat ", ")
                }
            )
    let func_st =
        pipe2
            getPosition
            (opt (stringReturn "pub" true .>> spaces1) .>> pstring "func" .>> spaces1
                .>>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (sepBy
                        (
                            spaces
                            >>. opt (attempt (stringReturn "*" 0uy))
                            .>>. ident
                            .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                        )
                        (spaces .>> pchar ',')
                    )
                .>>. opt (attempt (spaces >>. typp .>> spaces))
                .>>. block1 funcTerm
                .>> funcEndLines
            )
            (fun pos ((((isMod, name), args), rettyp), content) ->
                fast.add {
                    Type = "func_st"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s], arr[%s]]"
                        (match isMod with | Some v -> v | None -> false)
                        name
                        (match rettyp with | Some typ -> typ | None -> -1)
                        (args |> List.map (fun ((isrepo, f), s) -> sprintf "arr: [bool: %b, str: \"%s\", ref: %i]" (match isrepo with | Some _ -> true | None -> false) f (match s with | Some i -> i | None -> -1)) |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String .concat ", ")
                }
            )
    let func_pr =
        pipe2
            getPosition
            (opt (stringReturn "abs" true .>> spaces1) .>> pstring "func" .>> spaces1
                .>>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (sepBy
                        (
                            spaces
                            >>. opt (attempt (stringReturn "*" 0uy))
                            .>>. ident
                            .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                        )
                        (spaces .>> pchar ',')
                    )
                .>>. opt (attempt (spaces >>. typp .>> spaces))
                .>>. block1 funcTerm
                .>> funcEndLines
            )
            (fun pos ((((isMod, name), args), rettyp), content) ->
                fast.add {
                    Type = "func_pr"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s], arr[%s]]"
                        (match isMod with | Some v -> v | None -> false)
                        name
                        (match rettyp with | Some typ -> typ | None -> -1)
                        (args |> List.map (fun ((isrepo, f), s) -> sprintf "arr: [bool: %b, str: \"%s\", ref: %i]" (match isrepo with | Some _ -> true | None -> false) f (match s with | Some i -> i | None -> -1)) |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String .concat ", ")
                }
            )
    let initf =
        pipe2
            getPosition
            (opt (stringReturn "pub" true .>> spaces1) .>> pstring "init" .>> spaces1
                .>>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (sepBy
                        (
                            spaces
                            >>. opt (attempt (stringReturn "*" 0uy))
                            .>>. ident
                            .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                        )
                        (spaces .>> pchar ',')
                    )
                .>>. block1 (pstring "@" >>% -1)   // ToDO
                .>> funcEndLines
            )
            (fun pos (((isMod, name), args), content) ->
                fast.add {
                    Type = "init"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s], arr[%s]]"
                        (match isMod with | Some v -> v | None -> false)
                        name
                        (args |> List.map (fun ((isrepo, f), s) -> sprintf "arr: [bool: %b, str: \"%s\", ref: %i]" (match isrepo with | Some _ -> true | None -> false) f (match s with | Some i -> i | None -> -1)) |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String .concat ", ")
                }
            )

    let async_ =
        pipe2
            getPosition
            (
                pstring "async"
                >>. between
                    (spaces .>> pchar '(')
                    (pchar ')' .>> spaces)
                    (spaces >>. ident .>> spaces)
                .>>. opp.ExpressionParser
            )
            (fun pos (chan, content) ->
                fast.add {
                    Type = "async"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", ref: %i]"
                        chan
                        content
                }
            )

    let struct_ =
        pipe3
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)))
            (pstring "struct" .>> spaces1
                >>. ident
                .>>. opt (
                    attempt (
                        spaces1
                            .>> pstring "impl"
                            .>> spaces
                            >>. ident
                            .>>. opt (attempt (many (spaces .>> pchar ',' .>> spaces >>. ident)))
                    )
                )
                .>>. block
                    structTerm
                .>> stEndLines
            )
            (fun pos modi ((name, protocols), content) ->
                fast.add {
                    Type = "struct"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s], arr: [%s]]"
                        (match modi with | Some v -> v | None -> false)
                        name
                        ((match protocols with | Some (f, lst) -> [f] @ (match lst with | Some l -> l | None -> []) | None -> []) |> List.map (sprintf "str: \"%s\"") |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    
    let protocol_ =
        pipe3
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)))
            (pstring "protocol" .>> spaces1
                >>. ident
                .>>. block
                    protocolTerm
                .>> prEndLines
            )
            (fun pos modi (name, content) ->
                fast.add {
                    Type = "protocol"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s]]"
                        (match modi with | Some v -> v | None -> false)
                        name
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )

    let program =
        spaces
        >>. package_
        .>>. many import_
        .>>. many (achoice protocol_ [
            func_
            struct_
        ]) .>> eof
        |>> (fun ((package, imports), body) ->
            fast.add {
                Type = "program"
                Line = 1l
                Column = 1l
                Data = sprintf
                    "[ref: %i, arr: [%s], arr: [%s]]"
                    package
                    (imports |> List.map (sprintf "ref: %i") |> String.concat ", ")
                    (body |> List.map (sprintf "ref: %i") |> String.concat ", ")
            }
        )
    /// <summary>
    /// Left: false
    /// Right: true
    /// </summary>
    let oprators = [|
        [|
            "=", true
        |]
        [|
            "||", false
        |]
        [|
            "&&", false
        |]
        [|
            "==", false
            "!=", false
            ">=", false
            "<=", false
            ">", false
            "<", false
        |]
        [|
            "+", false
            "-", false
        |]
        [|
            "*", false
            "/", false
        |]
        [|
            "**", true
        |]
        [|
            ".", false
        |]
    |]

    do
        opp.TermParser <- exprTerm

        let adjustPosition offset (pos: Position) =
            Position(pos.StreamName, pos.Index + int64 offset,
                     pos.Line, pos.Column + int64 offset)
        let addOpr name prece assoc mapping =
            let op = InfixOperator(name, getPosition .>> spaces, prece, assoc, (), fun opPos lhs rhs -> mapping (adjustPosition -name.Length opPos) lhs rhs)
            opp.AddOperator op
        
        oprators
        |> Array.iter (fun ops ->
            ops
            |> Array.iteri (fun i (name, assoc) ->
                addOpr name (i + 1)
                    (if assoc then Assoc.Right else Assoc.Left)
                    (fun pos lhs rhs ->
                        fast.add {
                            Type = "operator_" + name
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf
                                "[ref: %i, ref: %i]"
                                lhs
                                rhs
                        }
                    )
            )
        )

        typRef.Value <-
            achoice
                (
                    pipe2
                        getPosition
                        ident
                        (fun pos s ->
                            if s = "_"
                            then -1
                            else fast.add {
                                Type = "type_s"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%s\"]" s
                            }
                        )
                )
                [
                    pipe3
                        getPosition
                        ident
                        (between
                            (spaces .>> pchar '<')
                            (pchar '>' .>> spaces)
                            (sepBy1 (spaces >>. typ) (spaces .>> pchar ','))
                        )
                        (fun pos f g ->
                            fast.add {
                                Type = "type_g"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%s\", arr: [%s]]" f (g |> List.map (sprintf "ref: %i") |> String.concat ", ")
                            }
                        )
                    pipe2
                        getPosition
                        (
                            pstring "chan" .>> spaces1 >>. typ
                        )
                        (fun pos s ->
                            fast.add {
                                Type = "type_chan"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[ref: %i]" s
                            }
                        )
                    pipe2
                        getPosition
                        (
                            pstring "func"
                            >>. between
                                (spaces .>> pchar '(')
                                (spaces .>> pchar ')')
                                (sepBy (spaces >>. typ) (spaces .>> pchar ','))
                            .>>. opt (attempt (spaces >>. typ .>> spaces))
                        )
                        (fun pos (arg, rettyp) ->
                            fast.add {
                                Type = "type_func"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf
                                    "[arr: [%s], ref: %i]"
                                    (arg |> List.map (sprintf "ref: %i") |> String.concat ", ")
                                    (match rettyp with | Some t -> t | None -> -1)
                            }
                        )
                ]
        funcTermRef.Value <- achoice
            opp.ExpressionParser [
            func_ .>> funcEndLines
            let_ .>> endLines
            async_ .>> endLines
        ] .>> endLines
        structTermRef.Value <- achoice
            (func_st .>> funcEndLines) [
            val_st .>> endLines
            initf .>> funcEndLines
        ]
        protocolTermRef.Value <- achoice
            (func_pr .>> funcEndLines) [
            val_pr .>> endLines
        ]
        exprTermref.Value <-
            achoice
                (
                    pipe2
                        getPosition
                        (
                            pchar '&'
                            .>> spaces
                            >>. exprTerm
                        )
                        (fun pos expr ->
                            fast.add {
                                Type = "com_ref"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[ref: %i]" expr
                            }
                        )
                )
                [
                    pipe2
                        getPosition
                        (pfloat .>> (pchar 'f' <|> pchar 'F'))
                        (fun pos value ->
                            fast.add {
                                Type = "operand_float"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%f\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (pint32 .>> opt (attempt (pchar 'l')))
                        (fun pos value ->
                            fast.add {
                                Type = "operand_int32"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (pint64 .>> attempt (pchar 'L'))
                        (fun pos value ->
                            fast.add {
                                Type = "operand_int64"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (puint32 .>> pchar 'u')
                        (fun pos value ->
                            fast.add {
                                Type = "operand_uint32"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (puint64 .>> pstring "UL")
                        (fun pos value ->
                            fast.add {
                                Type = "operand_uint64"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (pint16 .>> pchar 's')
                        (fun pos value ->
                            fast.add {
                                Type = "operand_int16"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (puint16 .>> pstring "us")
                        (fun pos value ->
                            fast.add {
                                Type = "operand_uint16"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (pint8 .>> pchar 'y')
                        (fun pos value ->
                            fast.add {
                                Type = "operand_int8"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (puint8 .>> pstring "uy")
                        (fun pos value ->
                            fast.add {
                                Type = "operand_uint8"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%i\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (between (pchar '"') (pchar '"') (manyStrings (regex @"\\?.")))
                        (fun pos value ->
                            fast.add {
                                Type = "operand_string"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%s\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (between (pchar '\'') (pchar '\'') (regex @"\\?."))
                        (fun pos value ->
                            fast.add {
                                Type = "operand_char"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%s\"]" value
                            }
                        )
                    pipe2
                        getPosition
                        (
                            pstring "func"
                            >>. between
                                (spaces .>> pchar '(')
                                (spaces .>> pchar ')' .>> spaces)
                                (sepBy (spaces >>. typp) (spaces .>> pchar ','))
                            .>>. opt (attempt (spaces >>. typp .>> spaces))
                            .>>. block1
                                funcTerm
                        )
                        (fun pos ((arg, rettyp), content) ->
                            fast.add {
                                Type = "operand_func"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf
                                    "[arr: [%s], ref: %i, arr: [%s]]"
                                    (arg |> List.map (sprintf "ref: %i") |> String.concat ", ")
                                    (match rettyp with | Some typ -> typ | None -> -1)
                                    (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                            }
                        )
                    pipe2
                        getPosition
                        (
                            pstring "chan"
                            .>> spaces1
                            >>. ident
                            .>>. between
                                (spaces .>> pchar '(')
                                (pchar ')')
                                (spaces >>. getPosition .>>. opt (attempt opp.ExpressionParser .>> spaces))
                        )
                        (fun pos (typ, (pos2, cap)) ->
                            fast.add {
                                Type = "chan"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf
                                    "[str: \"%s\", ref: %i]"
                                    typ
                                    (
                                        match cap with
                                        | Some cap -> cap
                                        | None ->
                                            fast.add {
                                                Type = "operand_int32"
                                                Line = pos2.Line
                                                Column = pos2.Column
                                                Data = sprintf "[str: \"1\"]"
                                            }
                                    )
                            }
                        )
                    pipe2
                        getPosition
                        (
                            ident
                            .>>. between
                                (spaces .>> pchar '(')
                                (spaces .>> pchar ')')
                                (sepBy (spaces >>. opp.ExpressionParser) (spaces .>> pchar ','))
                        )
                        (fun pos (name, args) ->
                            fast.add {
                                Type = "call_func"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%s\", arr: [%s]]" name (args |> List.map (sprintf "ref: %i") |> String.concat ", ")
                            }
                        )
                    variable
                    pipe2
                        getPosition
                        ident
                        (fun pos name ->
                            fast.add {
                                Type = "ident"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%s\"]" name
                            }
                        )
                    pipe2
                        getPosition
                        (between (pchar '(' .>> spaces) (spaces .>> pchar ')') opp.ExpressionParser)
                        (fun pos expr ->
                            fast.add {
                                Type = "paren"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[ref: %i]" expr
                            }
                        )
                    pipe2
                        getPosition
                        (block1 funcTerm)
                        (fun pos content ->
                            fast.add {
                                Type = "block"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf
                                    "[arr: [%s]]"
                                    (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                            }
                        )
                    pipe2
                        getPosition
                        (
                            pchar '*'
                            .>> spaces
                            >>. exprTerm
                        )
                        (fun pos expr ->
                            fast.add {
                                Type = "give_repo"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[ref: %i]" expr
                            }
                        )
                ]
                .>> spaces

    member _.Struct = struct_

    member _.run (s: string) =
        let s = s.Replace("\r", "")
        #if DEBUG
        printfn "parse: %A\n" s
        #endif
        match run program s with
        | Success(res, _, _) -> res
        | Failure(error, _, _) ->
            eprintfn "%s" error
            -1

    member _.getFlatAST() = fast