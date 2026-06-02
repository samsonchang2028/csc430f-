// For more information see https://aka.ms/fsharp-console-apps

module Program
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