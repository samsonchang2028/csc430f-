// For more information see https://aka.ms/fsharp-console-apps

module Program

// DATA DEFNS

// A Value is the result of evaluating an expression.
type Value =
    | NumV of float
    | BoolV of bool
    | StrV of string
    | CloV of string list * ExprC * Binding list
    | PrimV of string

// An ExprC is a parsed expression.
and ExprC =
    | NumC of float
    | IdC of string
    | StrC of string
    | IfC of ExprC * ExprC * ExprC
    | LamC of string list * ExprC
    | AppC of ExprC * ExprC list

// A Binding pairs a name with the value bound to it.
and Binding = Binding of string * Value

// An env is a list of bindings.
type Env = Binding list

let mtEnv : Env = []

// HELPERS

// extend-env : add one binding to the front of an environment.
let extendEnv (newBind : Binding) (oldEnv : Env) : Env =
    newBind :: oldEnv

// extend-env-many : add several param/value pairs to an environment at once.
let extendEnvMany (parms : string list) (vals : Value list) (oldEnv : Env) : Env =
    if List.length parms <> List.length vals then
        failwith (sprintf "VEBG: wrong arity, expected %A params but got %A args" parms vals)
    else
        List.fold2
            (fun (acc : Env) (param : string) (v : Value) -> extendEnv (Binding (param, v)) acc)
            oldEnv
            parms
            vals

// lookup : find the value bound to name, searching front to back.
let rec lookup (name : string) (env : Env) : Value =
    match env with
    | [] -> failwith (sprintf "VEBG: unbound identifier %s" name)
    | Binding (n, v) :: rest ->
        if name = n then v
        else lookup name rest

let topEnv : Env =
    [ Binding("+",         PrimV "+")
      Binding("-",         PrimV "-")
      Binding("*",         PrimV "*")
      Binding("/",         PrimV "/")
      Binding("<=",        PrimV "<=")
      Binding("substring", PrimV "substring")
      Binding("strlen",    PrimV "strlen")
      Binding("equal?",    PrimV "equal?")
      Binding("true",      BoolV true)
      Binding("false",     BoolV false)
      Binding("error",     PrimV "error") ]

// Format a float the way the language expects: whole numbers print
// without a trailing ".0", everything else prints normally.
let formatNum (n: float) : string =
    if n = floor n && not (System.Double.IsInfinity n) then
        string (int64 n)        // 34.0 -> "34", -7.0 -> "-7"
    else
        string n                // 3.14 -> "3.14"

// serialize : Value -> string
let serialize (v: Value) : string =
    match v with
    | NumV n      -> formatNum n
    | BoolV true  -> "true"
    | BoolV false -> "false"
    | StrV s      -> sprintf "\"%s\"" s
    | CloV _      -> "#<procedure>"
    | PrimV _     -> "#<primop>"

// Symbols that cannot be used as identifiers.
let reservedWords : string list =
    ["fn"; "->"; "if"; "="; "given"; "do"]

// is this name reserved?
let isReserved (s: string) : bool =
    List.contains s reservedWords

// does the list contain any reserved word?
let containsReserved (lst: string list) : bool =
    List.exists isReserved lst

// does the list contain a duplicate?
let rec containsDuplicate (lst: string list) : bool =
    match lst with
    | [] -> false
    | f :: r -> List.contains f r || containsDuplicate r

// INTERPRETER
// interp : ExprC -> Env -> Value
let rec interp e env =
    match e with
    | NumC n -> NumV n
    | IdC s -> lookup s env
    | StrC s -> StrV s
    | IfC (test, thn, els) ->
        match interp test env with
        | BoolV true -> interp thn env
        | BoolV false -> interp els env
        | _ -> failwith "if test was not a boolean"
    | LamC (parms, body) -> CloV (parms, body, env)
    | AppC (func, args) ->
        let funV = interp func env
        let argVs = args |> List.map (fun arg -> interp arg env)
        match funV with
        | CloV (parms, body, savedEnv) -> interp body (extendEnvMany parms argVs savedEnv)
        | _ -> failwith "application of non-function"

// TESTS

// extendEnv
assert (extendEnv (Binding ("b", NumV 2.0)) [Binding ("a", NumV 3.0)] = [Binding ("b", NumV 2.0); Binding ("a", NumV 3.0)])

// extendEnvMany
assert (extendEnvMany ["x"; "y"] [NumV 1.0; NumV 2.0] mtEnv = [Binding ("y", NumV 2.0); Binding ("x", NumV 1.0)])

// arity mismatch should raise
let arityRaised =
    try
        extendEnvMany ["x"; "y"] [NumV 1.0] mtEnv |> ignore
        false
    with _ -> true

assert arityRaised

// lookup
assert (lookup "x" [Binding ("x", NumV 5.0)] = NumV 5.0)
assert (lookup "x" [Binding ("x", NumV 1.0); Binding ("x", NumV 2.0)] = NumV 1.0)
assert (lookup "y" [Binding ("x", NumV 1.0); Binding ("y", NumV 2.0)] = NumV 2.0)

// unbound identifier should raise
let unboundRaised =
    try
        lookup "z" mtEnv |> ignore
        false
    with _ -> true

assert unboundRaised

printfn "All tests passed."
