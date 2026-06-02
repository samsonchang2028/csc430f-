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

// An ExprC is a parsed expression 
and ExprC =
    | NumC of float
    | IdC of string
    | StrC of string
    | IfC of ExprC * ExprC * ExprC               // test, then, else
    | LamC of string list * ExprC                // params, body
    | AppC of ExprC * ExprC list                 // function, arguments

// A Binding pairs a name with the value bound to it.
and Binding = Binding of string * Value

// An env is a list of bindings.
type Env = Binding list

let mtEnv : Env = []

//HELPERS

// extend-env : add one binding to the front of an environment.
let extendEnv (newBind: Binding) (oldEnv: Env) : Env =
    newBind :: oldEnv // :: is like cons

// extend-env-many : add several param/value pairs to an environment at once.
let extendEnvMany (parms: string list) (vals: Value list) (oldEnv: Env) : Env =
    if List.length parms <> List.length vals then
        failwith (sprintf "VEBG: wrong arity, expected %A params but got %A args" parms vals)
    else
        List.fold2
            (fun (acc: Env) (param: string) (v: Value) -> extendEnv (Binding(param, v)) acc)
            oldEnv
            parms
            vals

// lookup : find the value bound to name, searching front to back.
let rec lookup (name: string) (env: Env) : Value =
    match env with
    | [] -> failwith (sprintf "VEBG: unbound identifier %s" name)
    | Binding(n, v) :: rest ->
        if name = n then v
        else lookup name rest


// TESTS

// extendEnv
assert (extendEnv (Binding("b", NumV 2.0)) [Binding("a", NumV 3.0)] = [Binding("b", NumV 2.0); Binding("a", NumV 3.0)])

// extendEnvMany
assert (extendEnvMany ["x"; "y"] [NumV 1.0; NumV 2.0] mtEnv = [Binding("y", NumV 2.0); Binding("x", NumV 1.0)])

// arity mismatch should raise
let arityRaised =
    try extendEnvMany ["x"; "y"] [NumV 1.0] mtEnv |> ignore; false
    with _ -> true
assert arityRaised

// lookup
assert (lookup "x" [Binding("x", NumV 5.0)] = NumV 5.0)
// first match wins (shadowing)
assert (lookup "x" [Binding("x", NumV 1.0); Binding("x", NumV 2.0)] = NumV 1.0)
// finds binding further down the list
assert (lookup "y" [Binding("x", NumV 1.0); Binding("y", NumV 2.0)] = NumV 2.0)

// unbound identifier should raise
let unboundRaised =
    try lookup "z" mtEnv |> ignore; false
    with _ -> true
assert unboundRaised

printfn "All tests passed."