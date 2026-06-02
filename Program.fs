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

// An Env (environment) is just a list of bindings.
type Env = Binding list

let mtEnv : Env = []
