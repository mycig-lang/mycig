namespace Mycig.Compiler

open System

type InferredType =
    | TUnknown
    | TSelf
    | TNamed of string
    | TGeneric of string * InferredType list
    | TFunc of InferredType list * InferredType
    | TAbs of InferredType
    override this.ToString() =
        let rec render = function
            | TUnknown -> "_"
            | TSelf -> "Self"
            | TNamed name -> name
            | TGeneric(name, args) ->
                sprintf "%s<%s>" name (args |> List.map render |> String.concat ", ")
            | TFunc(args, ret) ->
                sprintf "func(%s) -> %s" (args |> List.map render |> String.concat ", ") (render ret)
            | TAbs inner -> sprintf "abs %s" (render inner)

        render this

type private FunctionInfo = {
    Name: string
    QualifiedName: string option
    Parameters: (string * InferredType) list
    ExplicitReturn: InferredType
    Body: int list
    SelfType: InferredType option
}

type TypeInference(fast: FlatAST) =
    let flatAST = fast.getAST() |> Array.indexed
    let mutable flatData = [||]
    let mutable frames = [||]
    let mutable frameNames = Set.empty<string>
    let mutable functionsByName = Map.empty<string, FunctionInfo>
    let mutable functionsByQualifiedName = Map.empty<string, FunctionInfo>
    let mutable functionReturnCache = Map.empty<string, InferredType>
    let mutable functionInferenceStack = Set.empty<string>
    let mutable nodeTypeCache = Map.empty<int, InferredType>

    let unitType = TNamed "unit"
    let boolType = TNamed "bool"
    let numericTypes =
        set [ "i8"; "i16"; "i32"; "i64"; "u8"; "u16"; "u32"; "u64"; "f64" ]

    let tryObjList (value: obj) =
        match value with
        | :? (obj list) as items -> Some items
        | _ -> None

    let getObjList (value: obj) =
        match tryObjList value with
        | Some items -> items
        | None -> []

    let getString (value: obj) =
        match value with
        | :? string as value -> value
        | _ -> ""

    let getInt (value: obj) =
        match value with
        | :? int as value -> value
        | _ -> -1

    let getOptList (value: obj) =
        match value with
        | :? (obj list option) as value -> value |> Option.defaultValue []
        | _ -> []

    let getNode index =
        match fast.getAST index with
        | Ok node -> node
        | Err msg -> raise <| IndexOutOfRangeException msg

    let getData index =
        if index < 0 || index >= flatData.Length then
            []
        else
            flatData[index] |> getObjList

    let rec mergeTypes left right =
        match left, right with
        | TUnknown, t
        | t, TUnknown -> t
        | TSelf, t
        | t, TSelf -> t
        | TNamed l, TNamed r when l = r -> left
        | TGeneric(ln, la), TGeneric(rn, ra) when ln = rn && la.Length = ra.Length ->
            TGeneric(ln, List.map2 mergeTypes la ra)
        | TFunc(la, lr), TFunc(ra, rr) when la.Length = ra.Length ->
            TFunc(List.map2 mergeTypes la ra, mergeTypes lr rr)
        | TAbs l, TAbs r -> TAbs(mergeTypes l r)
        | TNamed l, TNamed r when numericTypes.Contains l && numericTypes.Contains r ->
            if l = "f64" || r = "f64" then TNamed "f64"
            else left
        | _ when left = right -> left
        | _ -> left

    let rec resolveTypeRefInner selfType typeRef =
        match typeRef with
        | -2 ->
            match selfType with
            | Some t -> t
            | None -> TSelf
        | -1 -> TUnknown
        | idx ->
            let node = getNode idx
            let data = getData idx

            match node.Type, data with
            | "type", [ child ] -> resolveTypeRefInner selfType (getInt child)
            | "type_s", [ name ] -> TNamed(getString name)
            | "type_abs", [ inner ] -> TAbs(resolveTypeRefInner selfType (getInt inner))
            | "type_g", [ name; args ] ->
                TGeneric(
                    getString name,
                    args
                    |> getObjList
                    |> List.map (getInt >> resolveTypeRefInner selfType)
                )
            | "type_func", [ args; ret ] ->
                TFunc(
                    args
                    |> getObjList
                    |> List.map (getInt >> resolveTypeRefInner selfType),
                    resolveTypeRefInner selfType (getInt ret)
                )
            | _ -> TUnknown

    let makeQualifiedName selfTypeOpt name =
        match selfTypeOpt with
        | Some(TNamed selfName) -> Some(sprintf "%s::%s" selfName name)
        | Some(TGeneric(selfName, _)) -> Some(sprintf "%s::%s" selfName name)
        | _ -> None

    let parseParameters selfType (value: obj) =
        value
        |> getObjList
        |> List.map (fun entry ->
            let cols = getObjList entry
            let name =
                match cols with
                | _isImplicit :: name :: _typeRef :: _ -> getString name
                | _ -> ""

            let inferredType =
                match cols with
                | _isImplicit :: _name :: typeRef :: _ -> resolveTypeRefInner selfType (getInt typeRef)
                | _ -> TUnknown

            name, inferredType
        )

    let collectFunction name explicitReturn parameters body selfType =
        let info = {
            Name = name
            QualifiedName = makeQualifiedName selfType name
            Parameters = parameters
            ExplicitReturn = explicitReturn
            Body = body
            SelfType = selfType
        }

        functionsByName <- functionsByName |> Map.add name info

        match info.QualifiedName with
        | Some qualifiedName ->
            functionsByQualifiedName <- functionsByQualifiedName |> Map.add qualifiedName info
        | None -> ()

    let collectFunctions() =
        functionsByName <- Map.empty
        functionsByQualifiedName <- Map.empty

        flatAST
        |> Array.iter (fun (index, node) ->
            let data = getData index

            match node.Type, data with
            | "func", [ _isPub; name; retRef; parameters; body ] ->
                collectFunction
                    (getString name)
                    (resolveTypeRefInner None (getInt retRef))
                    (parseParameters None parameters)
                    (body |> getObjList |> List.map getInt)
                    None
            | "operand_func", [ parameters; retRef; _body ] ->
                collectFunction
                    (sprintf "@lambda_%d" index)
                    (resolveTypeRefInner None (getInt retRef))
                    (
                        parameters
                        |> getObjList
                        |> List.mapi (fun i arg -> sprintf "arg%d" i, resolveTypeRefInner None (getInt arg))
                    )
                    []
                    None
            | "impl", [ frameName; members ] ->
                let selfType = TNamed(getString frameName)

                members
                |> getObjList
                |> List.map getInt
                |> List.iter (fun memberRef ->
                    let memberNode = getNode memberRef
                    let memberData = getData memberRef

                    match memberNode.Type, memberData with
                    | "func_impl", [ _modi; name; retRef; parameters; body ] ->
                        collectFunction
                            (getString name)
                            (resolveTypeRefInner (Some selfType) (getInt retRef))
                            (parseParameters (Some selfType) parameters)
                            (body |> getObjList |> List.map getInt)
                            (Some selfType)
                    | "init", [ _isPub; _name; parameters; _body ] ->
                        collectFunction
                            "init"
                            selfType
                            (parseParameters (Some selfType) parameters)
                            []
                            (Some selfType)
                    | _ -> ()
                )
            | _ -> ()
        )

    let rec inferFunctionReturn info =
        let cacheKey = info.QualifiedName |> Option.defaultValue info.Name

        match Map.tryFind cacheKey functionReturnCache with
        | Some cached -> cached
        | None when functionInferenceStack.Contains cacheKey -> info.ExplicitReturn
        | None ->
            functionInferenceStack <- functionInferenceStack.Add cacheKey

            let environment =
                info.Parameters |> Map.ofList
                |> fun env ->
                    match info.SelfType with
                    | Some selfType -> env |> Map.add "self" selfType
                    | None -> env

            let inferredBodyReturn, _ = inferSequence info.Body environment info.SelfType
            let inferredReturn = mergeTypes info.ExplicitReturn inferredBodyReturn

            functionReturnCache <- functionReturnCache |> Map.add cacheKey inferredReturn
            functionInferenceStack <- functionInferenceStack.Remove cacheKey
            inferredReturn

    and inferBinaryOperator op left right =
        match op with
        | "==" | "!=" | "<" | ">" | "<=" | ">=" ->
            let _ = mergeTypes left right
            boolType
        | "&&" | "||" -> boolType
        | "+" | "-" | "*" | "/" | "%" -> mergeTypes left right
        | _ -> mergeTypes left right

    and inferCall name argTypes =
        let functionInfo =
            match Map.tryFind name functionsByQualifiedName with
            | Some info -> Some info
            | None ->
                match name.Split([| "::" |], StringSplitOptions.None) |> Array.tryLast with
                | Some shortName -> Map.tryFind shortName functionsByName
                | None -> Map.tryFind name functionsByName

        match functionInfo with
        | Some info ->
            let parameterTypes = info.Parameters |> List.map snd
            let _ =
                List.zip parameterTypes argTypes
                |> List.map (fun (expected, actual) -> mergeTypes expected actual)

            inferFunctionReturn info
        | None -> TUnknown

    and inferSequence refs environment selfType =
        ((unitType, environment), refs)
        ||> List.fold (fun (_, env) nodeRef ->
            inferStatement nodeRef env selfType
        )

    and inferStatement nodeRef environment selfType =
        let node = getNode nodeRef
        let data = getData nodeRef

        match node.Type, data with
        | "let", [ _isMut; _isRepo; name; typRef; content ] ->
            let inferredValueType, _ =
                inferSequence
                    (content |> getObjList |> List.map getInt)
                    environment
                    selfType

            let declaredType = resolveTypeRefInner selfType (getInt typRef)
            let finalType = mergeTypes declaredType inferredValueType
            finalType, environment |> Map.add (getString name) finalType
        | _ ->
            inferNodeInner nodeRef environment selfType, environment

    and inferNodeInner nodeRef environment selfType =
        match Map.tryFind nodeRef nodeTypeCache with
        | Some cached -> cached
        | None ->
            let node = getNode nodeRef
            let data = getData nodeRef

            let inferredType =
                match node.Type, data with
                | "operand_i8", _ -> TNamed "i8"
                | "operand_i16", _ -> TNamed "i16"
                | "operand_i32", _ -> TNamed "i32"
                | "operand_i64", _ -> TNamed "i64"
                | "operand_u8", _ -> TNamed "u8"
                | "operand_u16", _ -> TNamed "u16"
                | "operand_u32", _ -> TNamed "u32"
                | "operand_u64", _ -> TNamed "u64"
                | "operand_f64", _ -> TNamed "f64"
                | "operand_string", _ -> TNamed "string"
                | "operand_char", _ -> TNamed "char"
                | "ref_var", [ name ] ->
                    Map.tryFind (getString name) environment |> Option.defaultValue TUnknown
                | "ident", [ name ] ->
                    let identName = getString name
                    if frameNames.Contains identName then TNamed identName else TUnknown
                | "type", [ child ]
                | "paren", [ child ] ->
                    inferNodeInner (getInt child) environment selfType
                | "block", [ content ] ->
                    inferSequence
                        (content |> getObjList |> List.map getInt)
                        environment
                        selfType
                    |> fst
                | "if", [ cond; thenBody; elseIfs; elseBody ] ->
                    let _ = inferNodeInner (getInt cond) environment selfType |> mergeTypes boolType |> ignore

                    let branchTypes =
                        [
                            inferSequence
                                (thenBody |> getObjList |> List.map getInt)
                                environment
                                selfType
                            |> fst

                            yield!
                                elseIfs
                                |> getObjList
                                |> List.map getObjList
                                |> List.choose (function
                                    | elseifCond :: elseifBody :: _ ->
                                        let _ = inferNodeInner (getInt elseifCond) environment selfType |> mergeTypes boolType |> ignore
                                        Some(
                                            inferSequence
                                                (elseifBody |> getObjList |> List.map getInt)
                                                environment
                                                selfType
                                            |> fst
                                        )
                                    | _ -> None
                                )

                            if getOptList elseBody |> List.isEmpty |> not then
                                inferSequence
                                    (getOptList elseBody |> List.map getInt)
                                    environment
                                    selfType
                                |> fst
                        ]

                    match branchTypes with
                    | [] -> unitType
                    | head :: tail -> tail |> List.fold mergeTypes head
                | "call_func", [ name; args ] ->
                    inferCall
                        (getString name)
                        (
                            args
                            |> getObjList
                            |> List.map (fun arg -> inferNodeInner (getInt arg) environment selfType)
                        )
                | "operand_func", [ parameters; retRef; _body ] ->
                    TFunc(
                        parameters
                        |> getObjList
                        |> List.map (getInt >> resolveTypeRefInner selfType),
                        resolveTypeRefInner selfType (getInt retRef)
                    )
                | _ when node.Type.StartsWith("operator_") ->
                    match data with
                    | [ left; right ] ->
                        inferBinaryOperator
                            (node.Type["operator_".Length..])
                            (inferNodeInner (getInt left) environment selfType)
                            (inferNodeInner (getInt right) environment selfType)
                    | _ -> TUnknown
                | "func", [ _isPub; name; _retRef; _parameters; _body ] ->
                    inferCall (getString name) []
                | "func_impl", [ _modi; name; _retRef; _parameters; _body ] ->
                    match selfType with
                    | Some(TNamed selfName) -> inferCall (sprintf "%s::%s" selfName (getString name)) []
                    | _ -> inferCall (getString name) []
                | "init", _ ->
                    selfType |> Option.defaultValue TUnknown
                | _ -> TUnknown

            nodeTypeCache <- nodeTypeCache |> Map.add nodeRef inferredType
            inferredType

    member __.init() =
        fast.initData()
        flatData <- fast.getData()
        frames <- flatAST |> Array.filter (fun (_, node) -> node.Type = "frame")
        frameNames <-
            frames
            |> Array.choose (fun (index, _) ->
                match getData index with
                | _isPub :: name :: _ -> Some(getString name)
                | _ -> None
            )
            |> Set.ofArray

        functionReturnCache <- Map.empty
        functionInferenceStack <- Set.empty
        nodeTypeCache <- Map.empty
        collectFunctions()

    member this.getFrames() = frames

    member this.resolveTypeRef(typeRef: int) =
        resolveTypeRefInner None typeRef

    member this.inferNode(nodeRef: int) =
        inferNodeInner nodeRef Map.empty None

    member this.inferFunction(name: string) =
        let info =
            match Map.tryFind name functionsByQualifiedName with
            | Some fn -> Some fn
            | None -> Map.tryFind name functionsByName

        info |> Option.map inferFunctionReturn

    member this.inferAll() =
        flatAST
        |> Array.map (fun (index, node) -> index, node.Type, this.inferNode index)
