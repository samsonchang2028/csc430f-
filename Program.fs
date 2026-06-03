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
