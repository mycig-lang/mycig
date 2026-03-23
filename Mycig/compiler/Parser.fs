namespace Mycig.Compiler

open ParserModule

open FParsec

type Node = string * int64 * int64 * string

type Assoc = Associativity

/// <summary>
/// require type inference: -1
/// Self-Type             : -2
/// </summary>
type Parser() =
    let fast = FlatAST()
    let ss = SimpleScope()

    let funcTerm, funcTermRef = createParserForwardedToRef()
    let frameTerm, frameTermRef = createParserForwardedToRef()
    let typ, typRef = createParserForwardedToRef()
    let exprTerm, exprTermref = createParserForwardedToRef()
    
    let typp =
        choice [
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

    let variable = ident

    let block p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (
                pstring ""
                |>> (fun _ ->
                    ss.createScope()
                )
                >>. many p
                |>> (fun x ->
                    ss.deleteScope()
                    x
                )
            )
    let zblock p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (
                pstring ""
                |>> (fun _ ->
                    ss.createScope()
                )
                >>. p
                |>> (fun x ->
                    ss.deleteScope()
                    x
                )
            )
    let block1 p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (
                pstring ""
                |>> (fun _ ->
                    ss.createScope()
                )
                >>. many1 p
                |>> (fun x ->
                    ss.deleteScope()
                    x
                )
            )
    let funcBlock p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (many p)
    let funcBlock1 p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (many1 p)
    let blockOrExp =
        choice [
            block1 funcTerm
            spaces >>. exprTerm |>> (fun x -> [x])
        ]
    
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

    let if_ =
        pipe2
            getPosition
            (
                pstring "if"
                .>> spaces1
                >>. exprTerm
                .>>. funcBlock funcTerm
                .>> spaces
                .>>. opt (attempt (many (
                    pstring "else"
                    .>> spaces1
                    .>> pstring "if"
                    .>> spaces1
                    >>. exprTerm
                    .>>. funcBlock funcTerm
                    .>> spaces
                )))
                .>>. opt (
                    pstring "else"
                    .>> spaces
                    >>. funcBlock funcTerm
                )
            )
            (fun pos ((f, s), opt) ->
                fast.add {
                    Type = "if"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[ref: %i, arr: [%s], arr: [%s], opt: [%s]]"
                        (fst f)
                        (
                            snd f
                            |> List.map (sprintf "ref: %i")
                            |> String.concat ", "
                        )
                        (
                            s
                            |> function
                               | None -> []
                               | Some lst -> lst
                            |> List.map (fun (e, content) ->
                                sprintf
                                    "arr: [ref: %i, arr: [%s]]"
                                    e
                                    (
                                        content
                                        |> List.map (sprintf "ref: %i")
                                        |> String.concat ", "
                                    )
                            )
                            |> String.concat ", "
                        )
                        (
                            match opt with
                            | None -> ""
                            | Some v ->
                                v
                                |> List.map (sprintf "ref: %i")
                                |> String.concat ", "
                        )
                }
            )

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
                .>> endLines
            )
            (fun pos ((((ismut, isrepo), name), t), content) ->
                ss.add name
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
                        (
                            content
                            |> List.map (sprintf "ref: %i")
                            |> String.concat ", "
                        )
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
                    (
                        sepBy
                            (
                                spaces
                                >>. opt (attempt (stringReturn "*" 0uy))
                                .>>. ident
                                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                            )
                            (spaces .>> pchar ',')
                        |>> (fun lst ->
                            ss.createScope()
                            lst
                            |> List.iter (fun ((_, name), _) ->
                                ss.add name
                            )
                            lst
                        )
                    )
                .>>. opt (attempt (spaces .>> pstring "->" .>> spaces >>. typp .>> spaces))
                .>>. funcBlock1 funcTerm
                .>> funcEndLines
            )
            (fun pos ((((isMod, name), args), rettyp), content) ->
                ss.deleteScope()
                fast.add {
                    Type = "func"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s], arr: [%s]]"
                        (
                            match isMod with
                            | Some v -> v
                            | None -> false
                        )
                        name
                        (
                            match rettyp with
                            | Some typ -> typ
                            | None -> -1
                        )
                        (
                            args
                            |> List.map (fun ((idt, f), s) ->
                                sprintf
                                    "arr: [bool: %b, str: \"%s\", ref: %i]"
                                    (
                                        match idt with
                                        | Some _ -> true
                                        | None -> false
                                    )
                                    f
                                    (
                                        match s with
                                        | Some i -> i
                                        | None -> -1
                                    )
                            ) |> String.concat ", "
                        )
                        (
                            content
                            |> List.map (sprintf "ref: %i")
                            |> String .concat ", "
                        )
                }
            )
    let func_impl =
        pipe3
            getPosition
            (opt (attempt (pstring "pub" <|> pstring "abs" .>> spaces1)))
            (pstring "func" .>> spaces1
                >>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (
                        sepBy
                            (
                                spaces
                                >>. opt (attempt (stringReturn "*" 0uy))
                                .>>. ident
                                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                            )
                            (spaces .>> pchar ',')
                        |>> (fun lst ->
                            ss.createScope()
                            lst
                            |> List.iter (fun ((_, name), _) ->
                                ss.add name
                            )
                            lst
                        )
                    )
                .>>. opt (attempt (spaces .>> pstring "->" .>> spaces >>. typp .>> spaces))
                .>>. funcBlock1 funcTerm
                .>> funcEndLines
            )
            (fun pos isMod (((name, args), rettyp), content) ->
                ss.deleteScope()
                fast.add {
                    Type = "func_impl"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", str: \"%s\", ref: %i, arr: [%s], arr: [%s]]"
                        (match isMod with | Some v -> v | None -> "pri")
                        name
                        (match rettyp with | Some typ -> typ | None -> -1)
                        (
                            args
                            |> List.map (fun ((idt, f), s) ->
                                sprintf
                                    "arr: [bool: %b, str: \"%s\", ref: %i]"
                                    (match idt with | Some _ -> true | None -> false)
                                    f
                                    (
                                        match s with
                                        | Some i -> i
                                        | None ->
                                            if f = "self"
                                            then -2
                                            else -1
                                    )
                            )
                            |> String.concat ", "
                        )
                        (
                            content
                            |> List.map (sprintf "ref: %i")
                            |> String .concat ", "
                        )
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
                .>>. funcBlock (
                    (opt (attempt (stringReturn "*" true .>> spaces)))
                    .>>. ident
                    .>> (spaces .>> pchar ':' .>> spaces)
                    .>>. exprTerm
                    .>> (spaces .>> endLines .>> spaces)
                )
                .>> funcEndLines
            )
            (fun pos (((isMod, name), args), content) ->
                fast.add {
                    Type = "init"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s], arr: [%s]]"
                        (match isMod with | Some v -> v | None -> false)
                        name
                        (
                            args
                            |> List.map (fun ((idt, f), s) ->
                                sprintf
                                    "arr: [bool: %b, str: \"%s\", ref: %i]"
                                    (
                                        match idt with
                                        | Some _ -> true
                                        | None -> false
                                    )
                                    f
                                    (
                                        match s with
                                        | Some i -> i
                                        | None -> -1
                                    )
                            )
                            |> String.concat ", "
                        )
                        (
                            content
                            |> List.map (fun ((idt, f), s) ->
                                sprintf
                                    "arr: [bool: %b, str: \"%s\", ref: %i]"
                                    (
                                        match idt with
                                        | Some _ -> true
                                        | None -> false
                                    )
                                    f
                                    s
                            )
                            |> String .concat ", "
                        )
                }
            )
    let func_impl_abs =
        pipe2
            getPosition
            (
                pstring "abs"
                .>> spaces1
                .>> pstring "func"
                .>> spaces1
                >>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (
                        sepBy
                            (
                                spaces
                                >>. ident
                                .>>. (spaces .>> pchar ':' .>> spaces >>. typp)
                            )
                            (spaces .>> pchar ',')
                        |>> (fun lst ->
                            ss.createScope()
                            lst
                            |> List.iter (fun (name, _) ->
                                ss.add name
                            )
                            lst
                        )
                    )
                .>>. (spaces .>> pstring "->" .>> spaces >>. typp .>> spaces)
                .>> funcEndLines
            )
            (fun pos ((name, args), rettyp) ->
                ss.deleteScope()
                fast.add {
                    Type = "func_impl_abs"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", ref: %i, arr: [%s]]"
                        name
                        rettyp
                        (
                            args
                            |> List.map (fun (f, s) ->
                                sprintf
                                    "arr: [str: \"%s\", ref: %i]"
                                    f
                                    s
                            ) |> String.concat ", "
                        )
                }
            )
    let initf_abs =
        pipe2
            getPosition
            (pstring "abs" .>> spaces1 .>> pstring "init" .>> spaces1
                >>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (sepBy
                        (
                            spaces
                            >>. ident
                            .>>. (spaces .>> pchar ':' .>> spaces >>. typp)
                        )
                        (spaces .>> pchar ',')
                    )
                .>> funcEndLines
            )
            (fun pos (name, args) ->
                fast.add {
                    Type = "init_abs"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", arr: [%s]]"
                        name
                        (
                            args
                            |> List.map (fun (f, s) ->
                                sprintf
                                    "arr: [str: \"%s\", ref: %i]"
                                    f
                                    s
                            )
                            |> String.concat ", "
                        )
                }
            )
    let func_impl_pub =
        pipe2
            getPosition
            (
                pstring "pub"
                .>> spaces1
                .>> pstring "func"
                .>> spaces1
                >>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (
                        sepBy
                            (
                                spaces
                                >>. ident
                                .>>. (spaces .>> pchar ':' .>> spaces >>. typp)
                            )
                            (spaces .>> pchar ',')
                        |>> (fun lst ->
                            ss.createScope()
                            lst
                            |> List.iter (fun (name, _) ->
                                ss.add name
                            )
                            lst
                        )
                    )
                .>>. (spaces .>> pstring "->" .>> spaces >>. typp .>> spaces)
                .>> funcEndLines
            )
            (fun pos ((name, args), rettyp) ->
                ss.deleteScope()
                fast.add {
                    Type = "func_impl_pub"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", ref: %i, arr: [%s]]"
                        name
                        rettyp
                        (
                            args
                            |> List.map (fun (f, s) ->
                                sprintf
                                    "arr: [str: \"%s\", ref: %i]"
                                    f
                                    s
                            ) |> String.concat ", "
                        )
                }
            )
    let initf_pub =
        pipe2
            getPosition
            (pstring "pub" .>> spaces1 .>> pstring "init" .>> spaces1
                >>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (sepBy
                        (
                            spaces
                            >>. ident
                            .>>. (spaces .>> pchar ':' .>> spaces >>. typp)
                        )
                        (spaces .>> pchar ',')
                    )
                .>> funcEndLines
            )
            (fun pos (name, args) ->
                fast.add {
                    Type = "init_abs"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", arr: [%s]]"
                        name
                        (
                            args
                            |> List.map (fun (f, s) ->
                                sprintf
                                    "arr: [str: \"%s\", ref: %i]"
                                    f
                                    s
                            )
                            |> String.concat ", "
                        )
                }
            )

    let impl_ =
        pipe2
            getPosition
            (pstring "impl" .>> spaces1
                >>. ident
                .>>. block (
                    choice [
                        attempt initf
                        func_impl
                    ]
                )
            )
            (fun pos (name, content) ->
                fast.add {
                    Type = "impl"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", arr: [%s]]"
                        name
                        (
                            content
                            |> List.map (sprintf "ref: %i")
                            |> String.concat ", "
                        )

                }
            )
    let impl_abs =
        pipe3
            getPosition
            (pstring "abs" .>> spaces1)
            (pstring "impl"
                >>. block (
                    choice [
                        attempt initf_abs
                        func_impl_abs
                    ]
                )
            )
            (fun pos _ content ->
                fast.add {
                    Type = "impl_abs"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[arr: [%s]]"
                        (
                            content
                            |> List.map (sprintf "ref: %i")
                            |> String.concat ", "
                        )

                }
            )
    let impl_pub =
        pipe3
            getPosition
            (pstring "pub" .>> spaces1)
            (pstring "impl" .>> spaces1
                >>. ident
                .>>. block (
                    choice [
                        attempt initf_pub
                        func_impl_pub
                    ]
                )
            )
            (fun pos _ (name, content) ->
                fast.add {
                    Type = "impl_pub"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", arr: [%s]]"
                        name
                        (
                            content
                            |> List.map (sprintf "ref: %i")
                            |> String.concat ", "
                        )

                }
            )

    let field_ =
        pipe3
            getPosition
            (opt (attempt (pstring "pub" .>> spaces1)))
            (pstring "field" .>> spaces1
                >>. ident
                .>>. block (
                    (opt (attempt (stringReturn "*" true .>> spaces)))
                    .>>. ident
                    .>> (spaces .>> pchar ':' .>> spaces)
                    .>>. typp
                    .>> (spaces .>> endLines .>> spaces)
                )
            )
            (fun pos modi (name, content) ->
                fast.add {
                    Type = "field"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", str: \"%s\", arr: [%s]]"
                        (match modi with | Some v -> v | None -> "pri")
                        name
                        (
                            content
                            |> List.map (fun ((modi, name), t) ->
                                sprintf
                                    "arr: [bool: %b, str: \"%s\", ref: %i]"
                                    (match modi with | Some v -> v | None -> false)
                                    name
                                    t
                            )
                            |> String.concat ", "
                        )
                }
            )

    let frame_ =
        pipe3
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)))
            (pstring "frame" .>> spaces1
                >>. ident
                .>>. opt (
                    attempt (
                        spaces
                        .>> pstring ":"
                        .>> spaces
                        >>. sepBy1 (opt (attempt (pstring "pub" .>> spaces1)) .>>. ident) (attempt (spaces .>> pchar ',' .>> spaces))
                    )
                )
                .>>. block
                    frameTerm
                .>> frameEndLines
            )
            (fun pos modi ((name, frames), content) ->
                fast.add {
                    Type = "frame"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s], arr: [%s]]"
                        modi.IsSome
                        name
                        (
                            (
                                match frames with
                                | Some lst -> lst
                                | None -> []
                            )
                            |> List.map (fun (idt, name) ->
                                sprintf
                                    "arr: [bool: %b, str: \"%s\"]"
                                    idt.IsSome
                                    name
                            )
                            |> String.concat ", "
                        )
                        (
                            content
                            |> List.map (sprintf "ref: %i")
                            |> String.concat ", "
                        )
                }
            )

    let program =
        spaces
        >>. package_
        .>>. many import_
        .>>. many (choice [
            func_
            frame_
        ]) .>> eof
        |>> (fun ((package, imports), body) ->
            fast.add {
                Type = "program"
                Line = 1l
                Column = 1l
                Data = sprintf
                    "[ref: %i, arr: [%s], arr: [%s]]"
                    package
                    (
                        imports
                        |> List.map (sprintf "ref: %i")
                        |> String.concat ", "
                    )
                    (
                        body
                        |> List.map (sprintf "ref: %i")
                        |> String.concat ", "
                    )
            }
        )

    do
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

        let opp = OperatorPrecedenceParser()

        let expr = opp.ExpressionParser

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

        funcTermRef.Value <- choice [
            func_ .>> funcEndLines
            let_ .>> endLines
            if_ .>> endLines
            expr .>> endLines
        ] .>> endLines

        frameTermRef.Value <- choice [
            field_ .>> fieldEndLines
            impl_ .>> implEndLines
            impl_abs .>> implEndLines
            impl_pub .>> implEndLines
        ]

        typRef.Value <-
            choice [
                attempt (
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
                                Data = sprintf
                                    "[str: \"%s\", arr: [%s]]"
                                    f
                                    (
                                        g
                                        |> List.map (sprintf "ref: %i")
                                        |> String.concat ", "
                                    )
                            }
                        )
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
                                (
                                    arg
                                    |> List.map (sprintf "ref: %i")
                                    |> String.concat ", "
                                )
                                (
                                    match rettyp with
                                    | Some t -> t
                                    | None -> -1
                                )
                        }
                    )
                pipe2
                    getPosition
                    (
                        pstring "abs"
                        .>> spaces1
                        >>. typ
                    )
                    (fun pos t ->
                        fast.add {
                            Type = "type_abs"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf
                                "[ref: %i]"
                                t
                        }
                    )
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
                ]

        exprTermref.Value <-
            choice [
                pipe2
                    getPosition
                    (pint32 .>> opt (attempt (pstring "i32")))
                    (fun pos value ->
                        fast.add {
                            Type = "operand_i32"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%i\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (pint8 .>> pstring "i8")
                    (fun pos value ->
                        fast.add {
                            Type = "operand_i8"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%i\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (pint16 .>> pstring "i16")
                    (fun pos value ->
                        fast.add {
                            Type = "operand_i16"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%i\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (pint64 .>> attempt (pstring "i64"))
                    (fun pos value ->
                        fast.add {
                            Type = "operand_i64"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%i\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (pfloat .>> pstring "f64")
                    (fun pos value ->
                        fast.add {
                            Type = "operand_f64"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%f\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (puint8 .>> pstring "u8")
                    (fun pos value ->
                        fast.add {
                            Type = "operand_u8"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%i\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (puint16 .>> pstring "u16")
                    (fun pos value ->
                        fast.add {
                            Type = "operand_u16"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%i\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (puint32 .>> pstring "u32")
                    (fun pos value ->
                        fast.add {
                            Type = "operand_u32"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%i\"]" value
                        }
                    )
                pipe2
                    getPosition
                    (puint64 .>> pstring "u64")
                    (fun pos value ->
                        fast.add {
                            Type = "operand_u64"
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
                                (
                                    arg
                                    |> List.map (sprintf "ref: %i")
                                    |> String.concat ", "
                                )
                                (
                                    match rettyp with
                                    | Some typ -> typ
                                    | None -> -1
                                )
                                (
                                    content
                                    |> List.map (sprintf "ref: %i")
                                    |> String.concat ", "
                                )
                        }
                    )
                (
                    getPosition
                    .>>. variable
                    >>= (fun (pos, vName) ->
                        if ss.content vName then
                            fast.add {
                                Type = "ref_var"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[str: \"%s\"]" vName
                            } |> preturn
                        else fail <| sprintf "variable '%s' does not exist." vName
                    )
                )
                pipe2
                    getPosition
                    (
                        (sepBy1 ident (attempt (spaces .>> pstring "::" .>> spaces)))
                        .>>. between
                            (spaces .>> pchar '(')
                            (spaces .>> pchar ')')
                            (sepBy (spaces >>. expr) (spaces .>> pchar ','))
                    )
                    (fun pos (name, args) ->
                        fast.add {
                            Type = "call_func"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf
                                "[str: \"%s\", arr: [%s]]"
                                (
                                    name
                                    |> String.concat "::"
                                )
                                (
                                    args
                                    |> List.map (sprintf "ref: %i")
                                    |> String.concat ", "
                                )
                        }
                    )
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
                    (between (pchar '(' .>> spaces) (spaces .>> pchar ')') expr)
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
                                (
                                    content
                                    |> List.map (sprintf "ref: %i")
                                    |> String.concat ", "
                                )
                        }
                    )
                ]
                .>> spaces

    member __.run (s: string): int =
        let s = s.Replace("\r", "")
        #if DEBUG
        printfn "parse: %A\n" s
        #endif
        match run program s with
        | Success(res, _, _) -> res
        | Failure(error, _, _) ->
            eprintfn "%s" error
            -1

    member __.getFlatAST() = fast