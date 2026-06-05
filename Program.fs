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
let index_check (n : float) : bool =
    n >= 0.0 && floor n = n

// apply-primitive : String, listof value -> value
let apply_primitive (op : string)(args: Value list) : Value =
    match op with
    | "+" ->
        match args with 
        | [NumV l; NumV r] -> NumV (l + r)
        | _ -> failwith "VEBG: + expects two numbers"
    | "-" ->
        match args with 
        | [NumV l; NumV r] -> NumV (l - r)
        | _ -> failwith "VEBG: - expects two numbers"
    | "*" ->
        match args with 
        | [NumV l; NumV r] -> NumV (l * r)
        | _ -> failwith "VEBG: * expects two numbers"
    | "/" ->
        match args with 
        | [NumV l; NumV r] -> 
            if r = 0 then
                failwith "VEBG: / divide by zero error"
            else
                NumV (l / r)
        | _ -> failwith "VEBG: / expects two numbers"
    | "<=" ->
        match args with 
        | [NumV l; NumV r] -> BoolV (l <= r)
        | _ -> failwith "VEBG: <= expects two numbers"
    | "substring" ->
        match args with 
        | [StrV s; NumV start; NumV stop] ->   
            if not (index_check start) then
                failwith "VEBG: substring expects natural number"
            elif not (index_check stop) then
                failwith "VEBG: substring expects natural number"
            else
                let startId = int start
                let stopId = int stop 
                if (startId > s.Length) then
                    failwith "VEBG: substring start index out of range"
                elif (stopId > s.Length) then
                    failwith "VEBG: substring stop index out of range"
                elif (startId > stopId) then
                    failwith "VEBG: substring start index greater than stop index"
                else
                    (StrV (s.Substring(startId, stopId-startId)))
        | _ -> failwith "VEBG: substring expects string and two numbers"
    | "strlen" ->
        match args with 
        | [StrV s] -> NumV (float s.Length)
        | _ -> failwith "VEBG: strlen expects one string"
    | "equal?" -> 
        match args with
        | [NumV a; NumV b] -> BoolV (a=b)
        | [BoolV a; BoolV b] -> BoolV (a=b)
        | [StrV a; StrV b] -> BoolV (a=b)
        | [_; _] -> BoolV false
        | _ -> failwith "VEBG equal? expects exactly two arguments"
    | "error" -> 
        match args with
        |[v] -> failwith ("VEBG User Error" + string v)
        | _ -> failwith "VEBG error expected exactly one argument"
    | _ -> failwith "VEBG: unknown primitive operator"

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
        | PrimV op -> apply_primitive op argVs
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

assert (interp (AppC (IdC "+", [NumC 1.0; NumC 2.0])) topEnv = NumV 3.0)
assert (interp (AppC (IdC "-", [NumC 10.0; NumC 4.0])) topEnv = NumV 6.0)
assert (interp (AppC (IdC "*", [NumC 3.0; NumC 4.0])) topEnv = NumV 12.0)
assert (interp (AppC (IdC "/", [NumC 10.0; NumC 2.0])) topEnv = NumV 5.0)

let divZeroRaised =
    try
        interp (AppC (IdC "/", [NumC 1.0; NumC 0.0])) topEnv |> ignore
        false
    with _ -> true

assert divZeroRaised

assert (interp (AppC (IdC "<=", [NumC 1.0; NumC 2.0])) topEnv = BoolV true)
assert (interp (AppC (IdC "<=", [NumC 3.0; NumC 2.0])) topEnv = BoolV false)
assert (interp (AppC (IdC "<=", [NumC 2.0; NumC 2.0])) topEnv = BoolV true)

assert (interp (AppC (IdC "strlen", [StrC "hello"])) topEnv = NumV 5.0)
assert (interp (AppC (IdC "strlen", [StrC ""])) topEnv = NumV 0.0)

assert (interp (AppC (IdC "substring", [StrC "hello"; NumC 1.0; NumC 3.0])) topEnv = StrV "el")
assert (interp (AppC (IdC "substring", [StrC "hello"; NumC 0.0; NumC 5.0])) topEnv = StrV "hello")
assert (interp (AppC (IdC "substring", [StrC "hello"; NumC 0.0; NumC 0.0])) topEnv = StrV "")

let badSubstringRaised =
    try
        interp (AppC (IdC "substring", [StrC "hello"; NumC 3.0; NumC 1.0])) topEnv |> ignore
        false
    with _ -> true

assert badSubstringRaised

assert (interp (AppC (IdC "equal?", [NumC 1.0; NumC 1.0])) topEnv = BoolV true)
assert (interp (AppC (IdC "equal?", [NumC 1.0; NumC 2.0])) topEnv = BoolV false)
assert (interp (AppC (IdC "equal?", [BoolV true |> fun _ -> IdC "true"; IdC "true"])) topEnv = BoolV true)
assert (interp (AppC (IdC "equal?", [StrC "a"; StrC "a"])) topEnv = BoolV true)
assert (interp (AppC (IdC "equal?", [StrC "a"; StrC "b"])) topEnv = BoolV false)
assert (interp (AppC (IdC "equal?", [NumC 1.0; StrC "1"])) topEnv = BoolV false)

assert (interp (AppC (LamC (["x"], IdC "x"), [NumC 7.0])) topEnv = NumV 7.0)

let addTest =
    AppC(
        LamC(["x"; "y"], AppC(IdC "+", [IdC "x"; IdC "y"])),
        [NumC 3.0; NumC 4.0]
    )

assert (interp addTest topEnv = NumV 7.0)

let closureTestExpr =
    AppC (
        LamC (
            ["x"],
            AppC (
                LamC (["f"], AppC (IdC "f", [NumC 5.0])),
                [LamC (["y"], AppC (IdC "+", [IdC "x"; IdC "y"]))]
            )
        ),
        [NumC 10.0]
    )

assert (interp closureTestExpr topEnv = NumV 15.0)

let arityInterpRaised =
    try
        interp (AppC (LamC (["x"; "y"], IdC "x"), [NumC 1.0])) topEnv |> ignore
        false
    with _ -> true

assert arityInterpRaised

let nonFunctionRaised =
    try
        interp (AppC (NumC 5.0, [NumC 1.0])) topEnv |> ignore
        false
    with _ -> true

assert nonFunctionRaised

let userErrorRaised =
    try
        interp (AppC (IdC "error", [StrC "boom"])) topEnv |> ignore
        false
    with _ -> true

assert userErrorRaised

printfn "All tests passed."
