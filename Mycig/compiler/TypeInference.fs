namespace Mycig.Compiler

open System
open System.Text.RegularExpressions

type private InferredType =
    | TUnknown
    | TSelf
    | TNamed of string
    | TGeneric of string * InferredType list
    | TFunc of InferredType list * InferredType
    | TAbs of InferredType
    | TMut of InferredType
    | TRef of bool * InferredType
    override this.ToString() =
        let rec render =
            function
            | TUnknown -> "_"
            | TSelf -> "Self"
            | TNamed name -> name
            | TGeneric (name, args) -> sprintf "%s<%s>" name (args |> List.map render |> String.concat ", ")
            | TFunc (args, ret) -> sprintf "func(%s) -> %s" (args |> List.map render |> String.concat ", ") (render ret)
            | TAbs inner -> sprintf "abs %s" (render inner)
            | TMut inner -> sprintf "mut %s" (render inner)
            | TRef (true, inner) -> sprintf "&mut %s" (render inner)
            | TRef (false, inner) -> sprintf "& %s" (render inner)

        render this

type private FunctionInfo =
    { NodeRef: int
      Name: string
      QualifiedName: string option
      Parameters: (string * InferredType) list
      ExplicitReturn: InferredType
      Body: int list
      SelfType: InferredType option
      IsAbstract: bool }

type private FrameInfo =
    { Name: string
      Bases: (bool * string) list
      Fields: Map<string, InferredType> }

type TypeInference(fast: FlatAST) =
    let mutable flatAST = [||]
    let mutable flatData = [||]
    let mutable frames = [||]
    let mutable frameNames = Set.empty<string>
    let mutable functionsByName = Map.empty<string, FunctionInfo>
    let mutable functionsByQualifiedName = Map.empty<string, FunctionInfo>
    let mutable functionReturnCache = Map.empty<string, InferredType>
    let mutable functionInferenceStack = Set.empty<string>
    let mutable nodeTypeCache = Map.empty<int, InferredType>
    let mutable framesByName = Map.empty<string, FrameInfo>

    let unitType = TNamed "unit"
    let boolType = TNamed "bool"

    let numericTypes =
        set [ "i8"
              "i16"
              "i32"
              "i64"
              "u8"
              "u16"
              "u32"
              "u64"
              "f64" ]

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

    let getBool (value: obj) =
        match value with
        | :? bool as value -> value
        | _ -> false

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

    let renderData values = FlatAST.RenderData values

    let rec mergeTypes left right =
        match left, right with
        | TUnknown, t
        | t, TUnknown -> t
        | TSelf, t
        | t, TSelf -> t
        | TNamed l, TNamed r when l = r -> left
        | TGeneric (ln, la), TGeneric (rn, ra) when ln = rn && la.Length = ra.Length ->
            TGeneric(ln, List.map2 mergeTypes la ra)
        | TFunc (la, lr), TFunc (ra, rr) when la.Length = ra.Length ->
            TFunc(List.map2 mergeTypes la ra, mergeTypes lr rr)
        | TAbs l, TAbs r -> TAbs(mergeTypes l r)
        | TMut l, TMut r -> TMut(mergeTypes l r)
        | TRef (lm, l), TRef (rm, r) when lm = rm -> TRef(lm, mergeTypes l r)
        | TNamed l, TNamed r when numericTypes.Contains l && numericTypes.Contains r ->
            if l = "f64" || r = "f64" then
                TNamed "f64"
            else
                left
        | _ when left = right -> left
        | _ -> left

    let rec parseSimpleParameters selfType (value: obj) =
        value
        |> getObjList
        |> List.map (fun entry ->
            let cols = getObjList entry

            match cols with
            | argName :: typeRef :: _ -> getString argName, resolveTypeRefInner selfType (getInt typeRef)
            | _ -> "", TUnknown)

    and buildFrameTable () =
        framesByName <-
            frames
            |> Array.choose (fun (index, _) ->
                match getData index with
                | [ _isPub; name; bases; content ] ->
                    let fields =
                        content
                        |> getObjList
                        |> List.map getInt
                        |> List.choose (fun contentRef ->
                            let contentNode = getNode contentRef
                            let contentData = getData contentRef

                            match contentNode.Type, contentData with
                            | "field", [ _modi; _fieldBlockName; entries ] ->
                                Some(
                                    entries
                                    |> getObjList
                                    |> List.choose (fun entry ->
                                        let cols = getObjList entry

                                        match cols with
                                        | _isRepo :: fieldName :: typeRef :: _ ->
                                            Some(
                                                getString fieldName,
                                                resolveTypeRefInner (Some(TNamed(getString name))) (getInt typeRef)
                                            )
                                        | _ -> None)
                                )
                            | _ -> None)
                        |> List.concat
                        |> Map.ofList

                    let baseFrames =
                        bases
                        |> getObjList
                        |> List.choose (fun entry ->
                            let cols = getObjList entry

                            match cols with
                            | isPublic :: baseName :: _ -> Some(getBool isPublic, getString baseName)
                            | _ -> None)

                    Some(
                        getString name,
                        { Name = getString name
                          Bases = baseFrames
                          Fields = fields }
                    )
                | _ -> None)
            |> Map.ofArray

    and lookupFieldType visited frameName memberName =
        if visited |> Set.contains frameName then
            None
        else
            match Map.tryFind frameName framesByName with
            | Some frameInfo ->
                match Map.tryFind memberName frameInfo.Fields with
                | Some fieldType -> Some fieldType
                | None ->
                    frameInfo.Bases
                    |> List.tryPick (fun (_isPublic, baseName) ->
                        lookupFieldType (visited |> Set.add frameName) baseName memberName)
            | None -> None


    and resolveTypeRefInner selfType typeRef =
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
            | "type_bw_mut", [ inner ] -> TMut(resolveTypeRefInner selfType (getInt inner))
            | "type_bw_&", [ inner ] -> TRef(false, resolveTypeRefInner selfType (getInt inner))
            | "type_bw_&mut", [ inner ] -> TRef(true, resolveTypeRefInner selfType (getInt inner))
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
        | Some (TNamed selfName) -> Some(sprintf "%s::%s" selfName name)
        | Some (TGeneric (selfName, _)) -> Some(sprintf "%s::%s" selfName name)
        | _ -> None

    let rec buildTypeNodes wrapRoot startIndex inferredType line column : ASTNode list =
        let wrapTypeRoot (nodes: ASTNode list) =
            let innerRef = startIndex + nodes.Length - 1

            nodes
            @ [ { Type = "type"
                  Line = line
                  Column = column
                  Data = renderData [ box innerRef ] } ]

        let wrapNamed name =
            [ { Type = "type_s"
                Line = line
                Column = column
                Data = renderData [ box name ] } ]

        let nodes : ASTNode list =
            match inferredType with
            | TUnknown -> []
            | TSelf -> wrapNamed "Self"
            | TNamed name -> wrapNamed name
            | TAbs inner ->
                let innerNodes = buildTypeNodes false startIndex inner line column
                let innerRef = startIndex + innerNodes.Length - 1

                innerNodes
                @ [ { Type = "type_abs"
                      Line = line
                      Column = column
                      Data = renderData [ box innerRef ] } ]
            | TMut inner ->
                let innerNodes = buildTypeNodes false startIndex inner line column
                let innerRef = startIndex + innerNodes.Length - 1

                innerNodes
                @ [ { Type = "type_bw_mut"
                      Line = line
                      Column = column
                      Data = renderData [ box innerRef ] } ]
            | TRef (isMut, inner) ->
                let innerNodes = buildTypeNodes false startIndex inner line column
                let innerRef = startIndex + innerNodes.Length - 1

                innerNodes
                @ [ { Type = if isMut then "type_bw_&mut" else "type_bw_&"
                      Line = line
                      Column = column
                      Data = renderData [ box innerRef ] } ]
            | TGeneric (name, args) ->
                let argNodes, argRefs, _ =
                    (([], [], startIndex), args)
                    ||> List.fold (fun (nodes, refs, cursor) arg ->
                        let generated = buildTypeNodes false cursor arg line column

                        if List.isEmpty generated then
                            nodes, refs @ [ -1 ], cursor
                        else
                            let rootRef = cursor + generated.Length - 1
                            nodes @ generated, refs @ [ rootRef ], cursor + generated.Length)

                argNodes
                @ [ { Type = "type_g"
                      Line = line
                      Column = column
                      Data = renderData [ box name; box (argRefs |> List.map box) ] } ]
            | TFunc (args, ret) ->
                let argNodes, argRefs, cursor =
                    (([], [], startIndex), args)
                    ||> List.fold (fun (nodes, refs, current) arg ->
                        let generated = buildTypeNodes false current arg line column

                        if List.isEmpty generated then
                            nodes, refs @ [ -1 ], current
                        else
                            let rootRef = current + generated.Length - 1
                            nodes @ generated, refs @ [ rootRef ], current + generated.Length)

                let retNodes = buildTypeNodes false cursor ret line column
                let retRef =
                    if List.isEmpty retNodes then
                        -1
                    else
                        cursor + retNodes.Length - 1

                argNodes
                @ retNodes
                @ [ { Type = "type_func"
                      Line = line
                      Column = column
                      Data = renderData [ box (argRefs |> List.map box); box retRef ] } ]

        if wrapRoot && List.isEmpty nodes |> not then
            wrapTypeRoot nodes
        else
            nodes

    let materializeTypeNodes insertAt inferredType line column =
        let nodes = buildTypeNodes true insertAt inferredType line column

        if List.isEmpty nodes then
            None, []
        else
            Some(insertAt + nodes.Length - 1), nodes

    let rewriteNodeData (ast: ASTNode) returnRootRef selfRootRef =
        let values = FlatAST.ParseData ast.Data

        match ast.Type, values with
        | "let", [ isMut; isRepo; name; typRef; content ] when getInt typRef = -1 ->
            Some(
                renderData
                    [ isMut
                      isRepo
                      name
                      box (defaultArg returnRootRef -1)
                      content ]
            )
        | "func", [ isPub; name; retRef; parameters; body ] when getInt retRef = -1 ->
            Some(
                renderData
                    [ isPub
                      name
                      box (defaultArg returnRootRef -1)
                      parameters
                      body ]
            )
        | "func_impl", [ modi; name; retRef; parameters; body ] ->
            let selfTypeRef = defaultArg selfRootRef -1

            let rewrittenParameters =
                parameters
                |> getObjList
                |> List.map (fun entry ->
                    let cols = getObjList entry

                    match cols with
                    | isImplicit :: argName :: typeRef :: tail when getInt typeRef = -2 ->
                        box ([ isImplicit; argName; box selfTypeRef ] @ tail)
                    | _ -> entry)

            let rewrittenRetRef =
                if getInt retRef = -1 then
                    defaultArg returnRootRef -1
                else
                    getInt retRef

            let changed =
                rewrittenRetRef <> getInt retRef
                || (parameters
                    |> getObjList
                    |> List.exists (fun entry ->
                        match getObjList entry with
                        | _ :: _ :: typeRef :: _ -> getInt typeRef = -2
                        | _ -> false))

            if changed then
                Some(renderData [ modi; name; box rewrittenRetRef; box rewrittenParameters; body ])
            else
                None
        | "operand_func", [ parameters; retRef; body ] when getInt retRef = -1 ->
            Some(renderData [ parameters; box (defaultArg returnRootRef -1); body ])
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

            name, inferredType)

    let collectFunction nodeRef name qualifiedName explicitReturn parameters body selfType isAbstract =
        let info =
            { NodeRef = nodeRef
              Name = name
              QualifiedName = qualifiedName
              Parameters = parameters
              ExplicitReturn = explicitReturn
              Body = body
              SelfType = selfType
              IsAbstract = isAbstract }

        functionsByName <- functionsByName |> Map.add name info

        match info.QualifiedName with
        | Some qualifiedName ->
            functionsByQualifiedName <-
                functionsByQualifiedName
                |> Map.add qualifiedName info
        | None -> ()

    let collectFunctions () =
        functionsByName <- Map.empty
        functionsByQualifiedName <- Map.empty

        let addFrameMember ownerFrame targetFrame memberRef =
            let selfType = Some(TNamed ownerFrame)
            let memberNode = getNode memberRef
            let memberData = getData memberRef

            let qualified name = Some(sprintf "%s::%s" targetFrame name)

            match memberNode.Type, memberData with
            | "func_impl", [ _modi; name; retRef; parameters; body ] ->
                collectFunction
                    memberRef
                    (getString name)
                    (qualified (getString name))
                    (resolveTypeRefInner selfType (getInt retRef))
                    (parseParameters selfType parameters)
                    (body |> getObjList |> List.map getInt)
                    selfType
                    false
            | "func_impl_abs", [ name; retRef; parameters ] ->
                collectFunction
                    memberRef
                    (getString name)
                    (qualified (getString name))
                    (resolveTypeRefInner selfType (getInt retRef))
                    (parameters
                     |> getObjList
                     |> List.map (fun entry ->
                         let cols = getObjList entry

                         match cols with
                         | argName :: typeRef :: _ -> getString argName, resolveTypeRefInner selfType (getInt typeRef)
                         | _ -> "", TUnknown))
                    []
                    selfType
                    true
            | "func_impl_pub", [ name; retRef; parameters ] ->
                collectFunction
                    memberRef
                    (getString name)
                    (qualified (getString name))
                    (resolveTypeRefInner selfType (getInt retRef))
                    (parameters
                     |> getObjList
                     |> List.map (fun entry ->
                         let cols = getObjList entry

                         match cols with
                         | argName :: typeRef :: _ -> getString argName, resolveTypeRefInner selfType (getInt typeRef)
                         | _ -> "", TUnknown))
                    []
                    selfType
                    true
            | "init", [ _isPub; _name; parameters; _body ] ->
                collectFunction
                    memberRef
                    "init"
                    (qualified "init")
                    (TNamed ownerFrame)
                    (parseParameters selfType parameters)
                    []
                    selfType
                    false
            | "init_abs", [ _name; parameters ] ->
                collectFunction
                    memberRef
                    "init"
                    (qualified "init")
                    (TNamed ownerFrame)
                    (parameters
                     |> getObjList
                     |> List.map (fun entry ->
                         let cols = getObjList entry

                         match cols with
                         | argName :: typeRef :: _ -> getString argName, resolveTypeRefInner selfType (getInt typeRef)
                         | _ -> "", TUnknown))
                    []
                    selfType
                    true
            | _ -> ()

        let collectFrameMembers ownerFrame contentRef =
            let contentNode = getNode contentRef
            let contentData = getData contentRef

            match contentNode.Type, contentData with
            | "impl", [ targetFrame; members ] ->
                members
                |> getObjList
                |> List.map getInt
                |> List.iter (addFrameMember ownerFrame (getString targetFrame))
            | "impl_abs", [ members ] ->
                members
                |> getObjList
                |> List.map getInt
                |> List.iter (addFrameMember ownerFrame ownerFrame)
            | "impl_pub", [ targetFrame; members ] ->
                members
                |> getObjList
                |> List.map getInt
                |> List.iter (addFrameMember ownerFrame (getString targetFrame))
            | _ -> ()

        flatAST
        |> Array.iter (fun (index, node) ->
            let data = getData index

            match node.Type, data with
            | "func", [ _isPub; name; retRef; parameters; body ] ->
                collectFunction
                    index
                    (getString name)
                    None
                    (resolveTypeRefInner None (getInt retRef))
                    (parseParameters None parameters)
                    (body |> getObjList |> List.map getInt)
                    None
                    false
            | "operand_func", [ parameters; retRef; _body ] ->
                collectFunction
                    index
                    (sprintf "@lambda_%d" index)
                    None
                    (resolveTypeRefInner None (getInt retRef))
                    (parameters
                     |> getObjList
                     |> List.mapi (fun i arg -> sprintf "arg%d" i, resolveTypeRefInner None (getInt arg)))
                    []
                    None
                    false
            | "frame", [ _isPub; name; _bases; content ] ->
                content
                |> getObjList
                |> List.map getInt
                |> List.iter (collectFrameMembers (getString name))
            | _ -> ())

    let rec inferFunctionReturn info =
        let cacheKey =
            info.QualifiedName
            |> Option.defaultValue info.Name

        match Map.tryFind cacheKey functionReturnCache with
        | Some cached -> cached
        | None when functionInferenceStack.Contains cacheKey -> info.ExplicitReturn
        | None when info.IsAbstract -> info.ExplicitReturn
        | None ->
            functionInferenceStack <- functionInferenceStack.Add cacheKey

            let environment =
                info.Parameters
                |> Map.ofList
                |> fun env ->
                    match info.SelfType with
                    | Some selfType -> env |> Map.add "self" selfType
                    | None -> env

            let inferredBodyReturn, _ = inferSequence info.Body environment info.SelfType
            let inferredReturn = mergeTypes info.ExplicitReturn inferredBodyReturn

            functionReturnCache <-
                functionReturnCache
                |> Map.add cacheKey inferredReturn

            functionInferenceStack <- functionInferenceStack.Remove cacheKey
            inferredReturn

    and lookupMemberType frameName memberName =
        lookupFieldType Set.empty frameName memberName
        |> Option.orElseWith (fun () ->
            match Map.tryFind (sprintf "%s::%s" frameName memberName) functionsByQualifiedName with
            | Some info -> Some(inferFunctionReturn info)
            | None -> None)

    and inferBinaryOperator op left right =
        match op with
        | "=" -> mergeTypes left right
        | "=="
        | "!="
        | "<"
        | ">"
        | "<="
        | ">=" ->
            let _ = mergeTypes left right
            boolType
        | "&&"
        | "||" -> boolType
        | "." -> right
        | "+"
        | "-"
        | "*"
        | "/"
        | "%" -> mergeTypes left right
        | "**" ->
            match left, right with
            | TNamed "f64", _
            | _, TNamed "f64" -> TNamed "f64"
            | _ -> mergeTypes left right
        | _ -> mergeTypes left right

    and inferCall (name: string) (argTypes: InferredType list) =
        let functionInfo =
            match Map.tryFind name functionsByQualifiedName with
            | Some info -> Some info
            | None ->
                match
                    name.Split([| "::" |], StringSplitOptions.None)
                    |> Array.tryLast
                    with
                | Some shortName -> Map.tryFind shortName functionsByName
                | None -> Map.tryFind name functionsByName

        match functionInfo with
        | Some info ->
            let parameterTypes = info.Parameters |> List.map snd

            let _ =
                List.zip
                    (parameterTypes |> List.truncate argTypes.Length)
                    (argTypes |> List.truncate parameterTypes.Length)
                |> List.map (fun (expected, actual) -> mergeTypes expected actual)

            inferFunctionReturn info
        | None -> TUnknown

    and inferSequence refs environment selfType =
        ((unitType, environment), refs)
        ||> List.fold (fun (_, env) nodeRef -> inferStatement nodeRef env selfType)

    and inferStatement nodeRef environment selfType =
        let node = getNode nodeRef
        let data = getData nodeRef

        match node.Type, data with
        | "let", [ _isMut; _isRepo; name; typRef; content ] ->
            let inferredValueType, _ =
                inferSequence (content |> getObjList |> List.map getInt) environment selfType

            let declaredType = resolveTypeRefInner selfType (getInt typRef)
            let finalType = mergeTypes declaredType inferredValueType
            finalType, environment |> Map.add (getString name) finalType
        | _ -> inferNodeInner nodeRef environment selfType, environment

    and inferStatementType nodeRef environment selfType =
        inferStatement nodeRef environment selfType |> fst

    and tryInferInSequence target refs environment selfType =
        let rec loop env =
            function
            | [] -> None
            | nodeRef :: rest ->
                match tryInferInsideNode target nodeRef env selfType with
                | Some inferred -> Some inferred
                | None ->
                    let _, nextEnv = inferStatement nodeRef env selfType
                    loop nextEnv rest

        loop environment refs

    and tryInferInitBody target entries environment selfType =
        entries
        |> List.map getObjList
        |> List.choose (function
            | _isRepo :: _name :: exprRef :: _ -> Some(getInt exprRef)
            | _ -> None)
        |> List.tryPick (fun exprRef -> tryInferInsideNode target exprRef environment selfType)

    and tryInferInsideNode target nodeRef environment selfType =
        let node = getNode nodeRef
        let data = getData nodeRef

        if nodeRef = target then
            Some(inferStatementType nodeRef environment selfType)
        else
            match node.Type, data with
            | "type", [ child ]
            | "paren", [ child ] -> tryInferInsideNode target (getInt child) environment selfType
            | "bw_mut", [ child ]
            | "bw_&", [ child ]
            | "bw_&mut", [ child ] -> tryInferInsideNode target (getInt child) environment selfType
            | "call_func", [ _name; args ] ->
                args
                |> getObjList
                |> List.map getInt
                |> List.tryPick (fun arg -> tryInferInsideNode target arg environment selfType)
            | "block", [ content ] ->
                tryInferInSequence target (content |> getObjList |> List.map getInt) environment selfType
            | "let", [ _isMut; _isRepo; _name; _typRef; content ] ->
                tryInferInSequence target (content |> getObjList |> List.map getInt) environment selfType
            | "if", [ cond; thenBody; elseIfs; elseBody ] ->
                match tryInferInsideNode target (getInt cond) environment selfType with
                | Some inferred -> Some inferred
                | None ->
                    match tryInferInSequence target (thenBody |> getObjList |> List.map getInt) environment selfType
                        with
                    | Some inferred -> Some inferred
                    | None ->
                        let elseIfMatch =
                            elseIfs
                            |> getObjList
                            |> List.map getObjList
                            |> List.tryPick (function
                                | elseifCond :: elseifBody :: _ ->
                                    match tryInferInsideNode target (getInt elseifCond) environment selfType with
                                    | Some inferred -> Some inferred
                                    | None ->
                                        tryInferInSequence
                                            target
                                            (elseifBody |> getObjList |> List.map getInt)
                                            environment
                                            selfType
                                | _ -> None)

                        match elseIfMatch with
                        | Some inferred -> Some inferred
                        | None ->
                            tryInferInSequence target (getOptList elseBody |> List.map getInt) environment selfType
            | _ when node.Type.StartsWith("operator_") ->
                match data with
                | [ left; right ] ->
                    tryInferInsideNode target (getInt left) environment selfType
                    |> Option.orElseWith (fun () -> tryInferInsideNode target (getInt right) environment selfType)
                | _ -> None
            | "func", [ _isPub; _name; _retRef; parameters; body ] ->
                let functionEnv = parameters |> parseParameters None |> Map.ofList
                tryInferInSequence target (body |> getObjList |> List.map getInt) functionEnv None
            | "func_impl", [ _modi; _name; _retRef; parameters; body ] ->
                let functionEnv =
                    parameters
                    |> parseParameters selfType
                    |> Map.ofList
                    |> fun env ->
                        match selfType with
                        | Some self -> env |> Map.add "self" self
                        | None -> env

                tryInferInSequence target (body |> getObjList |> List.map getInt) functionEnv selfType
            | "func_impl_abs", [ _name; _retRef; parameters ] ->
                let functionEnv =
                    parameters
                    |> getObjList
                    |> List.map (fun entry ->
                        let cols = getObjList entry

                        match cols with
                        | argName :: typeRef :: _ -> getString argName, resolveTypeRefInner selfType (getInt typeRef)
                        | _ -> "", TUnknown)
                    |> Map.ofList
                    |> fun env ->
                        match selfType with
                        | Some self -> env |> Map.add "self" self
                        | None -> env

                if nodeRef = target then
                    Some(inferStatementType nodeRef environment selfType)
                else
                    None
            | "func_impl_pub", [ _name; _retRef; parameters ] ->
                let functionEnv =
                    parameters
                    |> getObjList
                    |> List.map (fun entry ->
                        let cols = getObjList entry

                        match cols with
                        | argName :: typeRef :: _ -> getString argName, resolveTypeRefInner selfType (getInt typeRef)
                        | _ -> "", TUnknown)
                    |> Map.ofList
                    |> fun env ->
                        match selfType with
                        | Some self -> env |> Map.add "self" self
                        | None -> env

                if nodeRef = target then
                    Some(inferStatementType nodeRef functionEnv selfType)
                else
                    None
            | "operand_func", [ parameters; _retRef; body ] ->
                let lambdaEnv =
                    parameters
                    |> getObjList
                    |> List.mapi (fun i arg -> sprintf "arg%d" i, resolveTypeRefInner selfType (getInt arg))
                    |> Map.ofList

                tryInferInSequence target (body |> getObjList |> List.map getInt) lambdaEnv selfType
            | "init", [ _isPub; _name; parameters; content ] ->
                let initEnv =
                    parameters
                    |> parseParameters selfType
                    |> Map.ofList
                    |> fun env ->
                        match selfType with
                        | Some self -> env |> Map.add "self" self
                        | None -> env

                tryInferInitBody target (content |> getObjList) initEnv selfType
            | "init_abs", [ _name; parameters ] ->
                let initEnv =
                    parameters
                    |> getObjList
                    |> List.map (fun entry ->
                        let cols = getObjList entry

                        match cols with
                        | argName :: typeRef :: _ -> getString argName, resolveTypeRefInner selfType (getInt typeRef)
                        | _ -> "", TUnknown)
                    |> Map.ofList
                    |> fun env ->
                        match selfType with
                        | Some self -> env |> Map.add "self" self
                        | None -> env

                None
            | "impl", [ _targetFrame; members ] ->
                members
                |> getObjList
                |> List.map getInt
                |> List.tryPick (fun memberRef -> tryInferInsideNode target memberRef environment selfType)
            | "impl_abs", [ members ] ->
                members
                |> getObjList
                |> List.map getInt
                |> List.tryPick (fun memberRef -> tryInferInsideNode target memberRef environment selfType)
            | "impl_pub", [ _targetFrame; members ] ->
                members
                |> getObjList
                |> List.map getInt
                |> List.tryPick (fun memberRef -> tryInferInsideNode target memberRef environment selfType)
            | "frame", [ _isPub; name; _bases; content ] ->
                let frameSelf = Some(TNamed(getString name))

                content
                |> getObjList
                |> List.map getInt
                |> List.tryPick (fun memberRef -> tryInferInsideNode target memberRef environment frameSelf)
            | "program", [ packageRef; imports; body ] ->
                tryInferInsideNode target (getInt packageRef) environment selfType
                |> Option.orElseWith (fun () ->
                    imports
                    |> getObjList
                    |> List.map getInt
                    |> List.tryPick (fun importRef -> tryInferInsideNode target importRef environment selfType))
                |> Option.orElseWith (fun () ->
                    body
                    |> getObjList
                    |> List.map getInt
                    |> List.tryPick (fun bodyRef -> tryInferInsideNode target bodyRef environment selfType))
            | _ -> None

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
                | "program", _ -> unitType
                | "package", _ -> unitType
                | "import", _ -> unitType
                | "frame", _ -> unitType
                | "field", _ -> unitType
                | "impl", _ -> unitType
                | "impl_abs", _ -> unitType
                | "impl_pub", _ -> unitType
                | "ref_var", [ name ] ->
                    Map.tryFind (getString name) environment
                    |> Option.defaultValue TUnknown
                | "ident", [ name ] ->
                    let identName = getString name

                    if frameNames.Contains identName then
                        TNamed identName
                    else
                        TUnknown
                | "type", [ child ]
                | "paren", [ child ] -> inferNodeInner (getInt child) environment selfType
                | "bw_mut", [ child ] -> TMut(inferNodeInner (getInt child) environment selfType)
                | "bw_&", [ child ] -> TRef(false, inferNodeInner (getInt child) environment selfType)
                | "bw_&mut", [ child ] -> TRef(true, inferNodeInner (getInt child) environment selfType)
                | "block", [ content ] ->
                    inferSequence (content |> getObjList |> List.map getInt) environment selfType
                    |> fst
                | "if", [ cond; thenBody; elseIfs; elseBody ] ->
                    let _ =
                        inferNodeInner (getInt cond) environment selfType
                        |> mergeTypes boolType
                        |> ignore

                    let branchTypes =
                        [ inferSequence (thenBody |> getObjList |> List.map getInt) environment selfType
                          |> fst

                          yield!
                              elseIfs
                              |> getObjList
                              |> List.map getObjList
                              |> List.choose (function
                                  | elseifCond :: elseifBody :: _ ->
                                      let _ =
                                          inferNodeInner (getInt elseifCond) environment selfType
                                          |> mergeTypes boolType
                                          |> ignore

                                      Some(
                                          inferSequence
                                              (elseifBody |> getObjList |> List.map getInt)
                                              environment
                                              selfType
                                          |> fst
                                      )
                                  | _ -> None)

                          if getOptList elseBody |> List.isEmpty |> not then
                              inferSequence (getOptList elseBody |> List.map getInt) environment selfType
                              |> fst ]

                    match branchTypes with
                    | [] -> unitType
                    | head :: tail -> tail |> List.fold mergeTypes head
                | "call_func", [ name; args ] ->
                    inferCall
                        (getString name)
                        (args
                         |> getObjList
                         |> List.map (fun arg -> inferNodeInner (getInt arg) environment selfType))
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
                        let op = node.Type["operator_".Length ..]

                        if op = "." then
                            let leftType = inferNodeInner (getInt left) environment selfType
                            let rightNode = getNode (getInt right)
                            let rightData = getData (getInt right)

                            match leftType, rightNode.Type, rightData with
                            | TNamed frameName, "ident", [ memberName ] when Map.containsKey frameName framesByName ->
                                lookupMemberType frameName (getString memberName)
                                |> Option.defaultValue TUnknown
                            | _ -> inferBinaryOperator op leftType (inferNodeInner (getInt right) environment selfType)
                        else
                            inferBinaryOperator
                                op
                                (inferNodeInner (getInt left) environment selfType)
                                (inferNodeInner (getInt right) environment selfType)
                    | _ -> TUnknown
                | "func", [ _isPub; name; _retRef; _parameters; _body ] -> inferCall (getString name) []
                | "func_impl", [ _modi; _name; retRef; _parameters; _body ] ->
                    resolveTypeRefInner selfType (getInt retRef)
                | "func_impl_abs", [ _name; retRef; _parameters ] -> resolveTypeRefInner selfType (getInt retRef)
                | "func_impl_pub", [ _name; retRef; _parameters ] -> resolveTypeRefInner selfType (getInt retRef)
                | "init", _ -> selfType |> Option.defaultValue TUnknown
                | "init_abs", _ -> selfType |> Option.defaultValue TUnknown
                | _ -> TUnknown

            nodeTypeCache <- nodeTypeCache |> Map.add nodeRef inferredType
            inferredType

    member this.init() =
        flatAST <- fast.getAST () |> Array.indexed
        fast.initData ()
        flatData <- fast.getData ()

        frames <-
            flatAST
            |> Array.filter (fun (_, node) -> node.Type = "frame")

        frameNames <-
            frames
            |> Array.choose (fun (index, _) ->
                match getData index with
                | _isPub :: name :: _ -> Some(getString name)
                | _ -> None)
            |> Set.ofArray

        buildFrameTable ()
        functionReturnCache <- Map.empty
        functionInferenceStack <- Set.empty
        nodeTypeCache <- Map.empty
        collectFunctions ()

    member __.getFlatAST() = fast

    member private this.getFrames() = frames

    member private this.resolveTypeRef(typeRef: int) = resolveTypeRefInner None typeRef

    member private this.inferNode(nodeRef: int) =
        nodeTypeCache <- Map.empty

        let inferredFromContext =
            flatAST
            |> Array.tryPick (fun (index, node) ->
                if node.Type = "program" then
                    tryInferInsideNode nodeRef index Map.empty None
                else
                    None)

        inferredFromContext
        |> Option.defaultWith (fun () -> inferNodeInner nodeRef Map.empty None)

    member private this.inferFunction(name: string) =
        let info =
            match Map.tryFind name functionsByQualifiedName with
            | Some fn -> Some fn
            | None -> Map.tryFind name functionsByName

        info |> Option.map inferFunctionReturn

    member this.materializeTypes() =
        let refRegex = Regex(@"ref:\s*(-?\d+)", RegexOptions.Compiled)

        let rewriteRefs canonicalIndex removedIndex (ast: ASTNode array) =
            ast
            |> Array.map (fun node ->
                let rewritten =
                    refRegex.Replace(
                        node.Data,
                        MatchEvaluator(fun m ->
                            let value = int m.Groups[1].Value

                            if value = removedIndex then
                                sprintf "ref: %i" canonicalIndex
                            elif value > removedIndex then
                                sprintf "ref: %i" (value - 1)
                            else
                                m.Value)
                    )

                { node with Data = rewritten })

        let deduplicateNodeType nodeType =
            let mutable ast = fast.getAST ()

            let duplicates =
                ast
                |> Array.indexed
                |> Array.choose (fun (index, node) ->
                    if node.Type = nodeType then
                        Some(node.Data, index)
                    else
                        None)
                |> Array.groupBy fst
                |> Array.collect (fun (_data, items) ->
                    items
                    |> Array.map snd
                    |> Array.sort
                    |> function
                        | [||]
                        | [| _ |] -> [||]
                        | sorted -> sorted[1..] |> Array.map (fun duplicate -> sorted[0], duplicate))
                |> Array.sortByDescending snd

            duplicates
            |> Array.iter (fun (canonicalIndex, duplicateIndex) ->
                ast <- rewriteRefs canonicalIndex duplicateIndex ast
                ast <- ast |> Array.removeAt duplicateIndex)

            fast.setAST ast
            fast.initData()

        let deduplicateTypeNodes () =
            deduplicateNodeType "type_s"
            deduplicateNodeType "type"

        let getInsertionIndex nodeRef nodeType data =
            let refsFrom value =
                value |> getObjList |> List.map getInt

            let firstRefOrNode value =
                refsFrom value
                |> List.tryHead
                |> Option.defaultValue nodeRef

            match nodeType, data with
            | "let", [ _isMut; _isRepo; _name; _typRef; content ] -> firstRefOrNode content
            | "func", [ _isPub; _name; _retRef; _parameters; body ] -> firstRefOrNode body
            | "func_impl", [ _modi; _name; _retRef; _parameters; body ] -> firstRefOrNode body
            | "operand_func", [ _parameters; _retRef; body ] -> firstRefOrNode body
            | _ -> nodeRef

        let tryFindFunctionInfoByNodeRef nodeRef =
            functionsByQualifiedName
            |> Map.values
            |> Seq.tryFind (fun info -> info.NodeRef = nodeRef)
            |> Option.orElseWith (fun () ->
                functionsByName
                |> Map.values
                |> Seq.tryFind (fun info -> info.NodeRef = nodeRef))

        let tryFindFrameSelfType nodeRef =
            frames
            |> Array.tryPick (fun (frameRef, _) ->
                match getData frameRef with
                | [ _isPub; frameName; _bases; content ] ->
                    let contentRefs = content |> getObjList |> List.map getInt

                    if contentRefs |> List.contains nodeRef then
                        Some(TNamed(getString frameName))
                    else
                        None
                | _ -> None)

        let targets =
            flatAST
            |> Array.choose (fun (index, node) ->
                let data = getData index

                match node.Type, data with
                | "let", [ _isMut; _isRepo; _name; typRef; _content ] when getInt typRef = -1 ->
                    let inferred = this.inferNode index

                    match inferred with
                    | TUnknown -> None
                    | _ -> Some(index, getInsertionIndex index node.Type data)
                | "func", [ _isPub; name; retRef; _parameters; _body ] when getInt retRef = -1 ->
                    this.inferFunction(getString name)
                    |> Option.bind (function
                        | TUnknown -> None
                        | _ -> Some(index, getInsertionIndex index node.Type data))
                | "func_impl", [ _modi; _name; retRef; parameters; _body ] ->
                    let needsReturn = getInt retRef = -1

                    let needsSelf =
                        parameters
                        |> getObjList
                        |> List.exists (fun entry ->
                            match getObjList entry with
                            | _ :: _ :: typeRef :: _ -> getInt typeRef = -2
                            | _ -> false)

                    if needsReturn || needsSelf then
                        Some(index, getInsertionIndex index node.Type data)
                    else
                        None
                | "operand_func", [ _parameters; retRef; _body ] when getInt retRef = -1 ->
                    Some(index, getInsertionIndex index node.Type data)
                | _ -> None)
            |> Array.sortByDescending snd

        let inserter = ASTInserter(fast)
        let mutable appliedInsertions : (int * int) list = []
        let mutable materializedTypeRoots = Map.empty<InferredType, int>

        let adjustIndex originalIndex =
            originalIndex
            + (appliedInsertions
               |> List.sumBy (fun (insertPos, count) ->
                   if insertPos <= originalIndex then
                       count
                   else
                       0))

        let shiftMaterializedRoots insertPos count =
            materializedTypeRoots <-
                materializedTypeRoots
                |> Map.map (fun _ rootRef ->
                    if rootRef >= insertPos then
                        rootRef + count
                    else
                        rootRef)

        let getOrMaterializeType insertPos inferredType line column =
            match inferredType with
            | TUnknown -> None, [], None
            | _ ->
                match Map.tryFind inferredType materializedTypeRoots with
                | Some rootRef -> Some rootRef, [], None
                | None ->
                    let rootRef, nodes = materializeTypeNodes insertPos inferredType line column

                    match rootRef with
                    | Some root -> Some root, nodes, Some(inferredType, root)
                    | None -> None, [], None

        targets
        |> Array.iter (fun (index, insertIndex) ->
            let currentIndex = adjustIndex index
            let currentInsertIndex = adjustIndex insertIndex
            let currentNode = (inserter.getAST ()).[currentIndex]
            let currentData = FlatAST.ParseData currentNode.Data

            let returnRootRef, returnNodes, selfRootRef, selfNodes =
                match currentNode.Type, currentData with
                | "let", [ _isMut; _isRepo; _name; typRef; _content ] when getInt typRef = -1 ->
                    let inferred = this.inferNode index

                    if inferred = TUnknown then
                        None, [], None, []
                    else
                        let rootRef, nodes, pending =
                            getOrMaterializeType currentInsertIndex inferred currentNode.Line currentNode.Column
                        pending |> Option.iter (fun (typ, root) -> materializedTypeRoots <- materializedTypeRoots |> Map.add typ root)
                        rootRef, nodes, None, []
                | "func", [ _isPub; name; retRef; _parameters; _body ] when getInt retRef = -1 ->
                    match this.inferFunction(getString name) with
                    | Some inferred when inferred <> TUnknown ->
                        let rootRef, nodes, pending =
                            getOrMaterializeType currentInsertIndex inferred currentNode.Line currentNode.Column
                        pending |> Option.iter (fun (typ, root) -> materializedTypeRoots <- materializedTypeRoots |> Map.add typ root)
                        rootRef, nodes, None, []
                    | _ -> None, [], None, []
                | "func_impl", [ _modi; _name; retRef; parameters; _body ] ->
                    let inferredReturn =
                        if getInt retRef = -1 then
                            tryFindFunctionInfoByNodeRef index
                            |> Option.map inferFunctionReturn
                            |> Option.defaultValue TUnknown
                        else
                            TUnknown

                    let inferredSelf =
                        let needsSelf =
                            parameters
                            |> getObjList
                            |> List.exists (fun entry ->
                                match getObjList entry with
                                | _ :: _ :: typeRef :: _ -> getInt typeRef = -2
                                | _ -> false)

                        if needsSelf then
                            tryFindFunctionInfoByNodeRef index
                            |> Option.bind (fun info -> info.SelfType)
                            |> Option.orElseWith (fun () -> tryFindFrameSelfType index)
                            |> Option.defaultValue TUnknown
                        else
                            TUnknown

                    let selfRoot, selfNodes, returnRoot, returnNodes =
                        match inferredSelf, inferredReturn with
                        | selfType, returnType when selfType <> TUnknown && returnType <> TUnknown && selfType = returnType ->
                            let sharedRoot, sharedNodes, pending =
                                getOrMaterializeType currentInsertIndex selfType currentNode.Line currentNode.Column

                            pending |> Option.iter (fun (typ, root) -> materializedTypeRoots <- materializedTypeRoots |> Map.add typ root)
                            sharedRoot, sharedNodes, sharedRoot, []
                        | selfType, returnType ->
                            let selfRoot, selfNodes =
                                if selfType = TUnknown then
                                    None, []
                                else
                                    let root, nodes, pending =
                                        getOrMaterializeType currentInsertIndex selfType currentNode.Line currentNode.Column
                                    pending |> Option.iter (fun (typ, registeredRoot) -> materializedTypeRoots <- materializedTypeRoots |> Map.add typ registeredRoot)
                                    root, nodes

                            let returnRoot, returnNodes =
                                if returnType = TUnknown then
                                    None, []
                                else
                                    let root, nodes, pending =
                                        getOrMaterializeType
                                            (currentInsertIndex + selfNodes.Length)
                                            returnType
                                            currentNode.Line
                                            currentNode.Column
                                    pending |> Option.iter (fun (typ, registeredRoot) -> materializedTypeRoots <- materializedTypeRoots |> Map.add typ registeredRoot)
                                    root, nodes

                            selfRoot, selfNodes, returnRoot, returnNodes

                    returnRoot, returnNodes, selfRoot, selfNodes
                | "operand_func", [ _parameters; retRef; _body ] when getInt retRef = -1 ->
                    let inferred = this.inferNode index

                    match inferred with
                    | TFunc (_, ret) when ret <> TUnknown ->
                        let rootRef, nodes, pending =
                            getOrMaterializeType currentInsertIndex ret currentNode.Line currentNode.Column
                        pending |> Option.iter (fun (typ, root) -> materializedTypeRoots <- materializedTypeRoots |> Map.add typ root)
                        rootRef, nodes, None, []
                    | _ -> None, [], None, []
                | _ -> None, [], None, []

            let insertedNodes = selfNodes @ returnNodes

            if List.isEmpty insertedNodes |> not then
                inserter.insertMany currentInsertIndex insertedNodes
                shiftMaterializedRoots (currentInsertIndex + insertedNodes.Length) insertedNodes.Length
                appliedInsertions <- (insertIndex, insertedNodes.Length) :: appliedInsertions

            let rewrittenIndex =
                if currentInsertIndex <= currentIndex then
                    currentIndex + insertedNodes.Length
                else
                    currentIndex
            let rewrittenNode = (inserter.getAST ()).[rewrittenIndex]

            match rewriteNodeData rewrittenNode returnRootRef selfRootRef with
            | Some data -> inserter.replace rewrittenIndex { rewrittenNode with Data = data }
            | None -> ())

        inserter.commit ()
        deduplicateTypeNodes ()
        this.init ()

    member private this.inferAll() =
        nodeTypeCache <- Map.empty

        flatAST
        |> Array.map (fun (index, node) -> index, node.Type, this.inferNode index)
