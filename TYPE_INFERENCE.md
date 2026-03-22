# Type Inference Specification

## 1. Purpose

`Mycig/compiler/TypeInference.fs` is responsible for deriving semantic types from the parser output stored in `FlatAST`.

The implementation does not operate on the source code text directly. It consumes:

1. `Parser` output
2. `FlatAST` node metadata
3. `FlatAST.initData()` decoded payloads

The goal of the component is to provide a practical type view for:

- program structure nodes
- explicit type references
- literals
- variables
- local bindings
- block expressions
- conditional expressions
- function calls
- function return types
- `impl` methods using `Self`

This document describes the current intended behavior of the implementation in detail.

## 2. Entry Point

The type inference engine is exposed through the `TypeInference` class.

Typical usage:

```fsharp
let parser = Parser()
parser.run code |> ignore

let fast = parser.getFlatAST()
let ti = TypeInference(fast)
ti.init()

let t1 = ti.inferNode(nodeId)
let t2 = ti.inferFunction("main")
let t3 = ti.resolveTypeRef(typeRefId)
```

`init()` must be called before any inference query.

## 3. Input Model

### 3.1 FlatAST

Each AST node has:

- `Type: string`
- `Line: int64`
- `Column: int64`
- `Data: string`

`Data` is stored as a serialized mini-structure and later decoded by `FlatAST.initData()` into boxed F# values.

### 3.2 Decoded Data Shapes

The inference engine reads decoded `Data` values with the following conventions:

- `bool: ...` becomes `bool`
- `str: ...` becomes `string`
- `ref: ...` becomes `int`
- `arr: [...]` becomes `obj list`
- `opt: [...]` becomes `obj list option`

The inference engine treats every node payload as `obj list` and then interprets each position according to the node kind.

## 4. Internal Type Model

The engine represents types by `InferredType`.

```fsharp
type InferredType =
    | TUnknown
    | TSelf
    | TNamed of string
    | TGeneric of string * InferredType list
    | TFunc of InferredType list * InferredType
    | TAbs of InferredType
```

Meaning of each case:

- `TUnknown`
  An unresolved type. This corresponds to parser sentinel `-1` or to cases where no reliable type can be derived.

- `TSelf`
  The special self type used when a type reference is `-2` outside of a bound concrete frame context.

- `TNamed name`
  A plain nominal type such as `i32`, `string`, `bool`, `MyFrame`.

- `TGeneric(name, args)`
  A generic nominal type such as `Vec<i32>`.

- `TFunc(args, ret)`
  A function type.

- `TAbs inner`
  A wrapper for abstract types represented by `abs T`.

## 5. Parser Sentinels

The parser emits two special type reference values.

- `-1`
  Means type inference is required.
  The engine maps this to `TUnknown`.

- `-2`
  Means `Self-Type`.
  The engine resolves this as:
  - the concrete frame type inside an `impl` context
  - `TSelf` if no concrete self type is available

## 6. Initialization Phase

`TypeInference.init()` performs the following steps:

1. Calls `fast.initData()`
2. Copies decoded payloads from `fast.getData()`
3. Collects all `frame` nodes
4. Collects frame names into a set
5. Clears caches
6. Collects function metadata

After `init()`, the engine is ready for type queries.

## 7. Function Metadata Collection

The engine builds a function table from the flat AST before performing expression inference.

### 7.1 Collected Sources

The following node kinds contribute function metadata:

- `func`
- `func_impl`
- `func_impl_abs`
- `init`
- `init_abs`
- `operand_func`

### 7.2 Stored Metadata

For each function-like node, the engine stores:

- function name
- optional qualified name
- parameter list
- explicit return type
- function body node references
- optional bound self type

### 7.3 Qualified Names

Methods in `impl FrameName { ... }` are given a qualified name:

```text
FrameName::method
```

This is required because `call_func` may refer to fully qualified names.

The unqualified name is also stored to support simple lookups, but the qualified name has higher priority during resolution.

### 7.4 Constructors

`init` is treated as a function whose return type is the concrete frame type of the surrounding `impl`.

The same rule is applied to `init_abs` inside `impl_abs`.

### 7.5 Abstract Members

Members collected from `impl_abs` are marked as abstract.

Their return types are not refined from the body during inference. The engine uses the explicit return type only.

## 8. Type Reference Resolution

The engine has a dedicated routine for parser-generated type nodes.

### 8.1 Supported Type Nodes

- `type`
- `type_s`
- `type_g`
- `type_func`
- `type_abs`

### 8.2 Resolution Rules

#### `type`

Wrapper node.

Payload:

```text
[ref: child]
```

Behavior:

- resolve the referenced child node

#### `type_s`

Simple named type.

Payload:

```text
[str: name]
```

Behavior:

- produce `TNamed name`

#### `type_g`

Generic type.

Payload:

```text
[str: name, arr: [ref: arg1, ref: arg2, ...]]
```

Behavior:

- resolve each type argument
- produce `TGeneric(name, args)`

#### `type_func`

Function type.

Payload:

```text
[arr: [ref: arg1, ref: arg2, ...], ref: ret]
```

Behavior:

- resolve all argument types
- resolve return type
- produce `TFunc(args, ret)`

#### `type_abs`

Abstract type wrapper.

Payload:

```text
[ref: inner]
```

Behavior:

- resolve the inner type
- produce `TAbs inner`

## 9. Merge Rules

The engine merges types when it must combine:

- declared and inferred types
- branch result types
- operator operand types
- function return annotation and inferred body type

### 9.1 Merge Priority

Current merge behavior is intentionally simple.

1. `TUnknown` yields to the other side
2. `TSelf` yields to the concrete other side
3. identical named types stay unchanged
4. generic types merge pointwise when names and arity match
5. function types merge pointwise when arity matches
6. abstract types merge their inner types
7. numeric named types may coalesce
8. otherwise the left side wins

### 9.2 Numeric Types

The numeric set currently includes:

- `i8`
- `i16`
- `i32`
- `i64`
- `u8`
- `u16`
- `u32`
- `u64`
- `f64`

When both sides are numeric:

- if either side is `f64`, result is `f64`
- otherwise the left side is retained

This is a pragmatic rule, not a full numeric promotion lattice.

## 10. Expression Inference

The engine walks flat AST nodes recursively.

Inference is environment-sensitive for local variables and `self`.

### 10.1 Literal Nodes

The following literal nodes map directly to nominal types:

- `operand_i8` -> `TNamed "i8"`
- `operand_i16` -> `TNamed "i16"`
- `operand_i32` -> `TNamed "i32"`
- `operand_i64` -> `TNamed "i64"`
- `operand_u8` -> `TNamed "u8"`
- `operand_u16` -> `TNamed "u16"`
- `operand_u32` -> `TNamed "u32"`
- `operand_u64` -> `TNamed "u64"`
- `operand_f64` -> `TNamed "f64"`
- `operand_string` -> `TNamed "string"`
- `operand_char` -> `TNamed "char"`

### 10.2 Variable Reference

#### `ref_var`

Payload:

```text
[str: name]
```

Behavior:

- look up the variable in the current local environment
- if found, use the bound type
- otherwise return `TUnknown`

### 10.3 Identifier

#### `ident`

Payload:

```text
[str: name]
```

Behavior:

- if the name matches a known `frame`, infer `TNamed name`
- otherwise return `TUnknown`

This reflects current implementation behavior. It is not yet a full symbol-resolution system.

### 10.4 Structural Nodes

The following nodes currently evaluate to `unit`:

- `program`
- `package`
- `import`
- `frame`
- `field`
- `impl`
- `impl_abs`

These nodes are treated as declarations or container structures, not as value-producing expressions.

### 10.5 Parenthesized Expression

#### `paren`

Payload:

```text
[ref: child]
```

Behavior:

- infer the child node directly

### 10.6 Block Expression

#### `block`

Payload:

```text
[arr: [ref: stmt1, ref: stmt2, ...]]
```

Behavior:

- evaluate contained nodes in order
- thread the local environment through the sequence
- the block result type is the type of the last node
- if the block is empty, use `unit`

### 10.7 Let Binding

#### `let`

Payload:

```text
[bool: isMut, bool: isRepo, str: name, ref: typeRef, arr: [ref: body...]]
```

Behavior:

1. infer the value/body expression type
2. resolve the declared type reference
3. merge declared type and inferred value type
4. bind the resulting type to the variable name in the current environment
5. return the final bound type as the node result

Notes:

- `isMut` and `isRepo` are currently ignored by the inference rules
- the binding becomes visible to subsequent nodes in the same block/function body

### 10.8 If Expression

#### `if`

Payload layout:

```text
[ref: cond, arr: [then...], arr: [elseif blocks...], opt: [else...]]
```

Behavior:

1. infer the condition and merge it against `bool`
2. infer the `then` branch result
3. infer all `else if` branch results
4. infer the `else` branch result if present
5. merge branch result types left-to-right
6. if there are no branches, use `unit`

Current behavior does not emit diagnostics on a condition mismatch. It only uses the merge rule.

### 10.9 Operators

Operator nodes are encoded as:

```text
operator_<symbol>
```

Payload:

```text
[ref: lhs, ref: rhs]
```

Behavior:

- infer both operands
- apply operator-specific result rules

Current operator rules:

- assignment operator `=`
  - result is `merge(lhs, rhs)`

- comparison operators `==`, `!=`, `<`, `>`, `<=`, `>=`
  - merge operand types
  - result is `bool`

- logical operators `&&`, `||`
  - result is `bool`

- arithmetic-like operators `+`, `-`, `*`, `/`, `%`
  - result is `merge(lhs, rhs)`

- exponent operator `**`
  - if either side is `f64`, result is `f64`
  - otherwise result is `merge(lhs, rhs)`

- member-like operator `.`
  - result is the inferred right-hand side type

- all other operators
  - result is `merge(lhs, rhs)`

The `.` rule is only a placeholder and does not perform real member resolution.

### 10.10 Function Call

#### `call_func`

Payload:

```text
[str: name, arr: [ref: arg1, ref: arg2, ...]]
```

Behavior:

1. infer all argument types
2. try exact lookup in qualified function table
3. if not found, try the final segment after `::`
4. if found:
   - compare parameter types against argument types via merge
   - infer the function return type
5. if not found:
   - return `TUnknown`

Current implementation does not produce an error for arity mismatch or incompatible arguments.

### 10.11 Function Value

#### `operand_func`

Payload:

```text
[arr: [ref: argType1, ref: argType2, ...], ref: retType, arr: [body...]]
```

Behavior:

- resolve each argument type
- resolve the return type
- produce `TFunc(args, ret)`

At the moment, the embedded anonymous function body is not used to refine the return type.

## 11. Function Return Inference

The engine can infer return types for named functions and methods.

### 11.1 Source of Return Type

Return type is derived from:

1. explicit return annotation
2. inferred last expression of the function body

The two are merged with the standard merge rule.

### 11.2 Local Environment

When inferring a function body, the environment initially contains:

- all parameters
- `self` if the function belongs to an `impl`

### 11.3 Recursion Guard

To avoid infinite recursion when functions depend on themselves directly or indirectly, the engine keeps an active inference stack.

If a function is encountered while already being inferred:

- the engine returns its explicit return type immediately

This means recursive functions without explicit annotation may remain partially unknown.

If the function is abstract, the engine also returns the explicit return type immediately.

## 12. Caching

The engine uses three caches:

- collected frame names
- function return cache
- node type cache

### 12.1 Purpose

- reduce repeated work
- stabilize return type queries
- avoid re-walking the same node structure

### 12.2 Limitation

`nodeTypeCache` is keyed only by node reference, not by environment.

This works for many simple trees because each node usually appears in a single lexical context, but it is still a simplification. If the same node were ever evaluated under different environments, the cache could become context-insensitive.

## 13. Public API

### `init() : unit`

Initializes decoded data, caches, frame table, and function table.

Must be called first.

### `getFrames()`

Returns collected `frame` nodes as indexed flat AST entries.

### `resolveTypeRef(typeRef: int) : InferredType`

Resolves a parser type reference into the internal type model.

### `inferNode(nodeRef: int) : InferredType`

Infers the type of a single node starting from an empty local environment.

### `inferFunction(name: string) : InferredType option`

Infers the return type of a function by name.

Accepted names:

- plain function name, for example `main`
- qualified method name, for example `Vec::push`

### `inferAll()`

Returns all nodes as:

```fsharp
(int * string * InferredType) array
```

Each tuple contains:

- node index
- node kind string
- inferred type

## 14. Unsupported or Partial Areas

The current implementation is deliberately lightweight. The following are not fully handled yet.

### 14.1 Diagnostics

The engine does not yet emit structured type errors such as:

- incompatible assignment
- invalid operator operand type
- invalid branch merge
- invalid return type
- invalid call arity
- unresolved symbol

It only produces best-effort `InferredType` values.

### 14.2 Full Symbol Resolution

`ident` is only recognized as a frame name. It is not yet resolved through:

- imports
- package paths
- function namespaces
- field access
- module symbols

### 14.3 Fields and Member Access

The engine currently collects frame names and `impl` methods, but it does not yet infer:

- field declarations into a field type table
- member access expressions
- constructor body effects

Even though operator `.` exists, it currently only forwards the right-hand side type and does not inspect fields or methods.

### 14.4 Anonymous Function Body Refinement

`operand_func` returns a `TFunc` based on declared argument and return references only.

The body is currently ignored for refinement.

### 14.5 Precise Numeric Promotion

Numeric merging is currently heuristic. A full specification would need:

- signed/unsigned conversion rules
- widening rules
- literal defaulting rules
- operator-specific numeric constraints

## 15. Recommended Future Extensions

To evolve this system into a stricter type checker, the next steps should be:

1. add a diagnostic type and collect errors/warnings
2. separate inference from validation
3. make node caching environment-aware where necessary
4. introduce a symbol table for modules, frames, fields, and functions
5. refine constructor and field inference
6. refine anonymous function inference from body expressions
7. define a proper numeric coercion lattice
8. define `unit` explicitly at the language level

## 16. Summary

The current type inference engine is a practical semantic layer on top of `FlatAST`.

It currently supports:

- program/package/import/frame/impl structural nodes
- parser type references
- scalar literals
- variables and local `let`
- blocks and `if`
- simple operator typing including `=`, `.`, and `**`
- named function and method return inference
- abstract `impl_abs` / `func_impl_abs` / `init_abs`
- `Self` resolution inside `impl`

It does not yet provide full type checking or diagnostics, but it establishes the structure required for that next stage.
