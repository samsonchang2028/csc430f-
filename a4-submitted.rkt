; Full project implemented
#lang typed/racket
(require typed/rackunit)

; ============================================================
; DATA DEFINITIONS
; ============================================================

; A Value is one of: Real numbers, booleans, strings, closures, primitive operators
(define-type Value (U NumV BoolV StrV CloV PrimV))

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)

; An ExprC is one of: NumC, IdC, StrC, IfC, LamC, AppC
(define-type ExprC (U NumC IdC StrC IfC LamC AppC))

(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct IfC ([test : ExprC] [thn : ExprC] [els : ExprC]) #:transparent)
(struct LamC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

; A Binding contains a name and value associated with that name
(struct Binding ([name : Symbol] [val : Value]) #:transparent)

; An Env is a list of Bindings
(define-type Env (Listof Binding))

(define mt-env : Env empty)

; ============================================================
; ENVIRONMENT HELPERS
; ============================================================

; extend-env : add one binding to an environment
(define (extend-env [new-bind : Binding] [old-env : Env]) : Env
  (cons new-bind old-env))

; extend-env-many : add multiple param/value pairs to an environment
(define (extend-env-many [params : (Listof Symbol)] [vals : (Listof Value)] [old-env : Env]) : Env
  (cond
    [(not (= (length params) (length vals)))
     (error 'interp "VEBG: wrong arity, expected ~e params but got ~e args" params vals)]
    [else
     (foldl
      (lambda ([param : Symbol] [val : Value] [acc : Env]) : Env
        (extend-env (Binding param val) acc))
      old-env
      params
      vals)]))

; lookup : find the value bound to name in env
(define (lookup [name : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "VEBG: unbound identifier ~e" name)]
    [(cons (Binding n v) rest)
     (if (symbol=? name n)
         v
         (lookup name rest))]))

; ============================================================
; TOP-LEVEL ENVIRONMENT
; ============================================================

(define top-env : Env
  (list
   (Binding '+ (PrimV '+))
   (Binding '- (PrimV '-))
   (Binding '* (PrimV '*))
   (Binding '/ (PrimV '/))
   (Binding '<= (PrimV '<=))
   (Binding 'substring (PrimV 'substring))
   (Binding 'strlen (PrimV 'strlen))
   (Binding 'equal? (PrimV 'equal?))
   (Binding 'true (BoolV #t))
   (Binding 'false (BoolV #f))
   (Binding 'error (PrimV 'error))))

; ============================================================
; SERIALIZE
; ============================================================

; serialize : Value -> String
; Convert a VEBG4 value to its string representation
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _) "#<primop>"]))

; ============================================================
; PARSE HELPERS
; ============================================================

; reserved-words : symbols that cannot be used as identifiers
(define reserved-words : (Listof Symbol)
  '(fn -> if = given do))

; is-reserved? : Symbol -> Boolean
(define (is-reserved? [s : Symbol]) : Boolean
  (if (member s reserved-words) #t #f))

; contains-reserved? : (Listof Symbol) -> Boolean
(define (contains-reserved? [lst : (Listof Symbol)]) : Boolean
  (match lst
    ['() #f]
    [(cons f r)
     (if (is-reserved? f) #t (contains-reserved? r))]))

; contains-duplicate? : (Listof Symbol) -> Boolean
(define (contains-duplicate? [lst : (Listof Symbol)]) : Boolean
  (match lst
    ['() #f]
    [(cons f r)
     (if (member f r) #t (contains-duplicate? r))]))

; ============================================================
; PARSER
; ============================================================

; parse : Sexp -> ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? string? str) (StrC str)]
    [(? symbol? sym)
     (if (is-reserved? sym)
         (error 'parse "VEBG: reserved identifier: ~e" sym)
         (IdC sym))]
    [(list 'if test thn els)
     (IfC (parse test) (parse thn) (parse els))]
    [(list 'given (list (list (? symbol? names) '= rhss) ...) 'do body)
     (define sym-names (cast names (Listof Symbol)))
     (define parsed-rhss (map parse (cast rhss (Listof Sexp))))
     (cond
       [(ormap is-reserved? sym-names)
        (error 'parse "VEBG: reserved binding name in: ~e" s)]
       [(contains-duplicate? sym-names)
        (error 'parse "VEBG: duplicate binding names in: ~e" s)]
       [else
        (AppC (LamC sym-names (parse body)) parsed-rhss)])]
    [(list 'given _ 'do _)
     (error 'parse "VEBG: invalid binding shape in: ~e" s)]
    [(list 'fn (list (? symbol? params) ...) '-> expr)
     (define sym-params (cast params (Listof Symbol)))
     (cond
       [(contains-reserved? sym-params)
        (error 'parse "VEBG: params contain reserved keyword in: ~e" s)]
       [(contains-duplicate? sym-params)
        (error 'parse "VEBG: duplicate parameter names in: ~e" s)]
       [else
        (LamC sym-params (parse expr))])]
    [(list 'fn ids '-> _)
     (error 'parse "VEBG: function parameters must be symbols in: ~e" s)]
    [(cons func args)
     (AppC (parse func) (map parse (cast args (Listof Sexp))))]
    [other (error 'parse "VEBG: bad expression syntax: ~e" other)]))

; ============================================================
; PRIMITIVE APPLICATION
; ============================================================

; index-check : Real -> Boolean
; Returns true if n is a non-negative exact integer
(define (index-check [n : Real]) : Boolean
  (and (exact-integer? n) (>= n 0)))

; apply-primitive : Symbol (Listof Value) -> Value
(define (apply-primitive [op : Symbol] [args : (Listof Value)]) : Value
  (match op
    ['+
     (match args
       [(list (NumV l) (NumV r)) (NumV (+ l r))]
       [_ (error 'interp "VEBG: + expects two numbers as arguments, given ~e" args)])]
    ['-
     (match args
       [(list (NumV l) (NumV r)) (NumV (- l r))]
       [_ (error 'interp "VEBG: - expects two numbers as arguments, given ~e" args)])]
    ['*
     (match args
       [(list (NumV l) (NumV r)) (NumV (* l r))]
       [_ (error 'interp "VEBG: * expects two numbers as arguments, given ~e" args)])]
    ['/
     (match args
       [(list (NumV l) (NumV r))
        (if (not (zero? r))
            (NumV (/ l r))
            (error 'interp "VEBG: division by zero not allowed, given ~e" args))]
       [_ (error 'interp "VEBG: / expects two numbers as arguments, given ~e" args)])]
    ['<=
     (match args
       [(list (NumV l) (NumV r)) (BoolV (<= l r))]
       [_ (error 'interp "VEBG: <= expects two numbers as arguments, given ~e" args)])]
    ['substring
     (match args
       [(list (StrV s) (NumV start) (NumV stop))
        (cond
          [(not (index-check start))
           (error 'interp "VEBG: substring expects natural number indices, got start ~e" start)]
          [(not (index-check stop))
           (error 'interp "VEBG: substring expects natural number indices, got stop ~e" stop)]
          [(> start (string-length s))
           (error 'interp "VEBG: substring start index out of range ~e for string ~e" start s)]
          [(> stop (string-length s))
           (error 'interp "VEBG: substring stop index out of range ~e for string ~e" stop s)]
          [(> start stop)
           (error 'interp "VEBG: substring start index greater than stop index in ~e" args)]
          [else
           (StrV (substring s (cast start Exact-Nonnegative-Integer)
                              (cast stop Exact-Nonnegative-Integer)))])]
       [_ (error 'interp "VEBG: substring expects a string and two numbers, given ~e" args)])]
    ['strlen
     (match args
       [(list (StrV s)) (NumV (string-length s))]
       [_ (error 'interp "VEBG: strlen expects a string, given ~e" args)])]
    ['equal?
     (match args
       [(list (NumV a) (NumV b))   (BoolV (equal? a b))]
       [(list (BoolV a) (BoolV b)) (BoolV (equal? a b))]
       [(list (StrV a) (StrV b))   (BoolV (equal? a b))]
       [(list _ _)                 (BoolV #f)]
       [_ (error 'interp "VEBG: equal? expects exactly two arguments, given ~e" args)])]
    ['error
     (match args
       [(list v) (error 'interp (string-append "VEBG user-error " (serialize v)))]
       [_ (error 'interp "VEBG: error expects exactly one argument, given ~e" args)])]
    [_ (error 'interp "VEBG: unknown primitive operator: ~e" op)]))

; ============================================================
; INTERPRETER
; ============================================================

; interp : ExprC Env -> Value
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(IdC s) (lookup s env)]
    [(IfC test thn els)
     (match (interp test env)
       [(BoolV #t) (interp thn env)]
       [(BoolV #f) (interp els env)]
       [v (error 'interp "VEBG: if test must evaluate to a boolean, got ~e in ~e" v e)])]
    [(LamC params body) (CloV params body env)]
    [(AppC func args)
     (define funcV (interp func env))
     (define argVs (map (lambda ([arg : ExprC]) (interp arg env)) args))
     (match funcV
       [(CloV params body saved-env)
        (interp body (extend-env-many params argVs saved-env))]
       [(PrimV op)
        (apply-primitive op argVs)]
       [_ (error 'interp "VEBG: func must evaluate to a function, got ~e in ~e" funcV e)])]))

; ============================================================
; TOP-INTERP
; ============================================================

; top-interp : Sexp -> String
; Parse and interpret an s-expression, returning a serialized result
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; ============================================================
; TESTS
; ============================================================

; --- extend-env ---
(check-equal? (extend-env (Binding 'b (NumV 2))
                          (list (Binding 'a (NumV 3)) (Binding 'c (NumV 5))))
              (list (Binding 'b (NumV 2)) (Binding 'a (NumV 3)) (Binding 'c (NumV 5))))

; --- extend-env-many ---
(check-exn #px"VEBG"
           (lambda () (extend-env-many '(x y) (list (NumV 1)) mt-env)))
(check-equal? (extend-env-many '(x y) (list (NumV 1) (NumV 2)) mt-env)
              (list (Binding 'y (NumV 2)) (Binding 'x (NumV 1))))

; --- lookup ---
(check-equal? (lookup '+ top-env) (PrimV '+))
(check-equal? (lookup 'x (list (Binding 'x (NumV 5)))) (NumV 5))
(check-equal? (lookup 'x (list (Binding 'x (NumV 1))
                               (Binding 'x (NumV 2)))) (NumV 1))
(check-equal? (lookup 'y (list (Binding 'x (NumV 1))
                               (Binding 'y (NumV 2)))) (NumV 2))
(check-exn #px"unbound identifier"
           (lambda () (lookup 'z mt-env)))

; --- serialize ---
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (NumV 3.14)) "3.14")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "This is a sentence.")) "\"This is a sentence.\"")
(check-equal? (serialize (StrV "csc")) "\"csc\"")
(check-equal? (serialize (CloV '(x) (NumC 1) mt-env)) "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")

; --- index-check ---
(check-equal? (index-check 4.5) #f)
(check-equal? (index-check 2) #t)
(check-equal? (index-check -1) #f)
(check-equal? (index-check 0) #t)

; --- apply-primitive ---
(check-equal? (apply-primitive '+ (list (NumV 1) (NumV 2))) (NumV 3))
(check-equal? (apply-primitive '- (list (NumV 5) (NumV 3))) (NumV 2))
(check-equal? (apply-primitive '* (list (NumV 3) (NumV 4))) (NumV 12))
(check-equal? (apply-primitive '/ (list (NumV 10) (NumV 2))) (NumV 5))
(check-exn #px"division by zero"
           (lambda () (apply-primitive '/ (list (NumV 1) (NumV 0)))))
(check-exn #px"\\+ expects two numbers"
           (lambda () (apply-primitive '+ (list (NumV 1) (BoolV #t)))))
(check-exn #px"\\+ expects two numbers"
           (lambda () (apply-primitive '+ (list (StrV "a") (NumV 1)))))
(check-exn #px"- expects two numbers"
           (lambda () (apply-primitive '- (list (BoolV #t) (NumV 1)))))
(check-exn #px"\\* expects two numbers"
           (lambda () (apply-primitive '* (list (NumV 1) (StrV "a")))))
(check-exn #px"/ expects two numbers"
           (lambda () (apply-primitive '/ (list (BoolV #f) (NumV 1)))))
(check-equal? (apply-primitive '<= (list (NumV 1) (NumV 2))) (BoolV #t))
(check-equal? (apply-primitive '<= (list (NumV 3) (NumV 2))) (BoolV #f))
(check-equal? (apply-primitive '<= (list (NumV 2) (NumV 2))) (BoolV #t))
(check-exn #px"<= expects two numbers"
           (lambda () (apply-primitive '<= (list (NumV 1) (BoolV #t)))))
(check-exn #px"<= expects two numbers"
           (lambda () (apply-primitive '<= (list (StrV "a") (NumV 1)))))
(check-equal? (apply-primitive 'equal? (list (NumV 1) (NumV 1))) (BoolV #t))
(check-equal? (apply-primitive 'equal? (list (BoolV #t) (BoolV #t))) (BoolV #t))
(check-equal? (apply-primitive 'equal? (list (BoolV #f) (BoolV #f))) (BoolV #t))
(check-equal? (apply-primitive 'equal? (list (StrV "a") (StrV "a"))) (BoolV #t))
(check-equal? (apply-primitive 'equal? (list (NumV 1) (NumV 2))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (BoolV #t) (BoolV #f))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (StrV "a") (StrV "b"))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (NumV 1) (BoolV #t))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (NumV 1) (StrV "1"))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (BoolV #t) (StrV "true"))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (CloV '() (NumC 1) mt-env)
                                             (CloV '() (NumC 1) mt-env))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (CloV '() (NumC 1) mt-env)
                                             (NumV 1))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (NumV 1)
                                             (CloV '() (NumC 1) mt-env))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (PrimV '+) (PrimV '+))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (PrimV '+) (NumV 1))) (BoolV #f))
(check-equal? (apply-primitive 'equal? (list (NumV 1) (PrimV '+))) (BoolV #f))
(check-exn #px"equal\\? expects exactly two arguments"
           (lambda () (apply-primitive 'equal? (list (NumV 1)))))
(check-exn #px"equal\\? expects exactly two arguments"
           (lambda () (apply-primitive 'equal? (list (NumV 1) (NumV 2) (NumV 3)))))
(check-equal? (apply-primitive 'strlen (list (StrV "hello"))) (NumV 5))
(check-equal? (apply-primitive 'strlen (list (StrV ""))) (NumV 0))
(check-exn #px"strlen expects a string"
           (lambda () (apply-primitive 'strlen (list (NumV 5)))))
(check-exn #px"strlen expects a string"
           (lambda () (apply-primitive 'strlen (list (StrV "a") (StrV "b")))))
(check-equal? (apply-primitive 'substring (list (StrV "hello") (NumV 1) (NumV 3)))
              (StrV "el"))
(check-exn #px"substring expects natural number indices"
           (lambda () (apply-primitive 'substring (list (StrV "hi") (NumV 1.5) (NumV 2)))))
(check-exn #px"substring expects natural number indices"
           (lambda () (apply-primitive 'substring (list (StrV "hi") (NumV 0) (NumV 1.5)))))
(check-exn #px"substring expects natural number indices"
           (lambda () (apply-primitive 'substring (list (StrV "hi") (NumV -1) (NumV 2)))))
(check-exn #px"start index out of range"
           (lambda () (apply-primitive 'substring (list (StrV "hi") (NumV 3) (NumV 4)))))
(check-exn #px"stop index out of range"
           (lambda () (apply-primitive 'substring (list (StrV "hi") (NumV 0) (NumV 5)))))
(check-exn #px"start index greater than stop"
           (lambda () (apply-primitive 'substring (list (StrV "hello") (NumV 3) (NumV 1)))))
(check-exn #px"substring expects a string and two numbers"
           (lambda () (apply-primitive 'substring (list (NumV 1) (NumV 0) (NumV 1)))))
(check-exn #px"user-error"
           (lambda () (apply-primitive 'error (list (NumV 42)))))
(check-exn #px"user-error.*true"
           (lambda () (apply-primitive 'error (list (BoolV #t)))))
(check-exn #px"user-error.*hello"
           (lambda () (apply-primitive 'error (list (StrV "hello")))))
(check-exn #px"error expects exactly one argument"
           (lambda () (apply-primitive 'error (list (NumV 1) (NumV 2)))))
(check-exn #px"unknown primitive operator"
           (lambda () (apply-primitive 'ohhello (list (NumV 1)))))

; --- parse ---
(check-equal? (parse '5) (NumC 5))
(check-equal? (parse '3.14) (NumC 3.14))
(check-equal? (parse '-7) (NumC -7))
(check-equal? (parse '0) (NumC 0))
(check-equal? (parse '"hello") (StrC "hello"))
(check-equal? (parse '"") (StrC ""))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse 'foo) (IdC 'foo))
(check-equal? (parse '+) (IdC '+))
(check-equal? (parse 'true) (IdC 'true))
(check-exn #px"reserved identifier" (lambda () (parse 'fn)))
(check-exn #px"reserved identifier" (lambda () (parse 'if)))
(check-exn #px"reserved identifier" (lambda () (parse '->)))
(check-exn #px"reserved identifier" (lambda () (parse '=)))
(check-exn #px"reserved identifier" (lambda () (parse 'given)))
(check-exn #px"reserved identifier" (lambda () (parse 'do)))
(check-exn #px"reserved" (lambda () (parse '(fn (if) -> 5))))
(check-exn #px"reserved" (lambda () (parse '(fn (fn) -> 5))))
(check-exn #px"reserved" (lambda () (parse '(fn (x ->) -> x))))
(check-equal? (parse '(if true 1 2))
              (IfC (IdC 'true) (NumC 1) (NumC 2)))
(check-equal? (parse '(if (<= 3 4) "yes" "no"))
              (IfC (AppC (IdC '<=) (list (NumC 3) (NumC 4)))
                   (StrC "yes") (StrC "no")))
(check-equal? (parse '(fn (x y) -> (+ x y)))
              (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))))
(check-equal? (parse '(fn () -> 5))
              (LamC '() (NumC 5)))
(check-equal? (parse '(fn (x) -> x))
              (LamC '(x) (IdC 'x)))
(check-exn #px"duplicate parameter"
           (lambda () (parse '(fn (x x) -> x))))
(check-exn #px"parameters must be symbols"
           (lambda () (parse '(fn (1 2) -> 5))))
(check-equal? (parse '(given ((x = 5)) do x))
              (AppC (LamC '(x) (IdC 'x)) (list (NumC 5))))
(check-equal? (parse '(given ((x = 1) (y = 2)) do (+ x y)))
              (AppC (LamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
                    (list (NumC 1) (NumC 2))))
(check-equal? (parse '(given () do 5))
              (AppC (LamC '() (NumC 5)) '()))
(check-exn #px"duplicate binding"
           (lambda () (parse '(given ((x = 1) (x = 2)) do x))))
(check-exn #px"reserved binding name"
           (lambda () (parse '(given ((if = 1)) do if))))
(check-exn #px"invalid binding shape"
           (lambda () (parse '(given ((x 1)) do x))))
(check-equal? (parse '(f 1 2))
              (AppC (IdC 'f) (list (NumC 1) (NumC 2))))
(check-equal? (parse '(f))
              (AppC (IdC 'f) '()))
(check-equal? (parse '((fn (x) -> x) 5))
              (AppC (LamC '(x) (IdC 'x)) (list (NumC 5))))
(check-exn #px"bad expression syntax" (lambda () (parse '#t)))
(check-exn #px"bad expression syntax" (lambda () (parse '#f)))

; --- interp ---
(check-equal? (interp (NumC 5) top-env) (NumV 5))
(check-equal? (interp (NumC 0) top-env) (NumV 0))
(check-equal? (interp (StrC "hi") top-env) (StrV "hi"))
(check-equal? (interp (StrC "") top-env) (StrV ""))
(check-equal? (interp (IdC 'true) top-env) (BoolV #t))
(check-equal? (interp (IdC 'false) top-env) (BoolV #f))
(check-equal? (interp (IdC '+) top-env) (PrimV '+))
(check-equal? (interp (IdC 'x) (extend-env (Binding 'x (NumV 42)) top-env)) (NumV 42))
(check-exn #px"unbound identifier"
           (lambda () (interp (IdC 'z) top-env)))
(check-equal? (interp (IfC (IdC 'true) (NumC 1) (NumC 2)) top-env) (NumV 1))
(check-equal? (interp (IfC (IdC 'false) (NumC 1) (NumC 2)) top-env) (NumV 2))
(check-exn #px"must evaluate to a boolean"
           (lambda () (interp (IfC (NumC 0) (NumC 1) (NumC 2)) top-env)))
(check-exn #px"must evaluate to a boolean"
           (lambda () (interp (IfC (StrC "true") (NumC 1) (NumC 2)) top-env)))
(check-equal? (interp (LamC '(x) (IdC 'x)) top-env)
              (CloV '(x) (IdC 'x) top-env))
(check-equal? (interp (LamC '() (NumC 5)) mt-env)
              (CloV '() (NumC 5) mt-env))
(check-equal? (interp (AppC (LamC '(x) (IdC 'x)) (list (NumC 7))) top-env) (NumV 7))
(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 2))) top-env) (NumV 3))
(check-exn #px"must evaluate to a function"
           (lambda () (interp (AppC (NumC 5) (list (NumC 1))) top-env)))
(check-exn #px"must evaluate to a function"
           (lambda () (interp (AppC (StrC "hi") (list (NumC 1))) top-env)))
(check-exn #px"must evaluate to a function"
           (lambda () (interp (AppC (IdC 'true) (list (NumC 1))) top-env)))
(check-exn #px"wrong arity"
           (lambda () (interp (AppC (LamC '(x y) (IdC 'x))
                                    (list (NumC 1))) top-env)))
(check-exn #px"wrong arity"
           (lambda () (interp (AppC (LamC '(x) (IdC 'x))
                                    (list (NumC 1) (NumC 2))) top-env)))

; --- top-interp ---
(check-equal? (top-interp '5) "5")
(check-equal? (top-interp '"hello") "\"hello\"")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")
(check-equal? (top-interp '+) "#<primop>")
(check-equal? (top-interp '(+ 1 2)) "3")
(check-equal? (top-interp '(- 10 4)) "6")
(check-equal? (top-interp '(* 3 4)) "12")
(check-equal? (top-interp '(/ 10 2)) "5")
(check-equal? (top-interp '(+ (* 2 3) (- 10 4))) "12")
(check-equal? (top-interp '(* (+ 1 2) (+ 3 4))) "21")
(check-equal? (top-interp '(<= 3 5)) "true")
(check-equal? (top-interp '(<= 5 3)) "false")
(check-equal? (top-interp '(<= 3 3)) "true")
(check-equal? (top-interp '(equal? 1 1)) "true")
(check-equal? (top-interp '(equal? 1 2)) "false")
(check-equal? (top-interp '(equal? "a" "a")) "true")
(check-equal? (top-interp '(equal? true false)) "false")
(check-equal? (top-interp '(strlen "hello")) "5")
(check-equal? (top-interp '(substring "hello" 1 3)) "\"el\"")
(check-equal? (top-interp '(if true 1 2)) "1")
(check-equal? (top-interp '(if false 1 2)) "2")
(check-equal? (top-interp '(if (<= 1 2) "yes" "no")) "\"yes\"")
(check-equal? (top-interp '(if (<= 5 2) "yes" "no")) "\"no\"")
(check-equal? (top-interp '((fn (x) -> (+ x 1)) 5)) "6")
(check-equal? (top-interp '((fn (x y) -> (+ x y)) 3 4)) "7")
(check-equal? (top-interp '((fn () -> 42))) "42")
(check-equal? (top-interp '(given ((x = 5)) do x)) "5")
(check-equal? (top-interp '(given ((x = 5) (y = 3)) do (+ x y))) "8")
(check-equal? (top-interp '(given () do 42)) "42")
(check-equal? (top-interp '(given ((+ = 5)) do +)) "5")
(check-equal? (top-interp '(given ((true = 0)) do true)) "0")
(check-equal? (top-interp '(given ((x = 10))
                             do ((fn (y) -> (+ x y)) 5))) "15")
(check-equal? (top-interp '(given ((make-adder = (fn (n) -> (fn (x) -> (+ n x)))))
                             do ((make-adder 10) 5))) "15")
(check-equal? (top-interp '(given ((apply = (fn (f x) -> (f x))))
                             do (apply (fn (n) -> (+ n 1)) 10))) "11")
(check-equal? (top-interp '(given ((compose = (fn (f g) -> (fn (x) -> (f (g x))))))
                             do ((compose (fn (x) -> (* x 2))
                                          (fn (x) -> (+ x 1))) 3))) "8")
(check-equal? (top-interp
               '(given ((factorial =
                         (fn (self n) ->
                             (if (<= n 0) 1 (* n (self self (- n 1)))))))
                  do (factorial factorial 5))) "120")
(check-equal? (top-interp '(given ((x = 1))
                             do (given ((y = 2))
                                  do (+ x y)))) "3")
(check-exn #px"unbound identifier"
           (lambda () (top-interp '(given ((x = y) (y = 5)) do (+ x y)))))
(check-equal? (top-interp '(given ((double = (fn (x) -> (* x 2))))
                             do (+ (double 3) (double 5)))) "16")
(check-equal? (top-interp '(given ((pick = (fn (b) ->
                                               (if b
                                                   (fn (x) -> (+ x 1))
                                                   (fn (x) -> (* x 2))))))
                             do ((pick true) 5))) "6")
(check-equal? (top-interp '(given ((pick = (fn (b) ->
                                               (if b
                                                   (fn (x) -> (+ x 1))
                                                   (fn (x) -> (* x 2))))))
                             do ((pick false) 5))) "10")
(check-equal? (top-interp '(+ (+ (+ 1 2) (+ 3 4)) (+ (+ 5 6) (+ 7 8)))) "36")
(check-equal? (top-interp '(equal? (fn (x) -> x) (fn (x) -> x))) "false")
(check-equal? (top-interp '(equal? + +)) "false")
(check-equal? (top-interp '(fn (x) -> x)) "#<procedure>")
(check-exn #px"division by zero"
           (lambda () (top-interp '(/ 1 0))))
(check-exn #px"user-error"
           (lambda () (top-interp '(error 42))))
(check-exn #px"user-error.*hello"
           (lambda () (top-interp '(error "hello"))))
(check-exn #px"must evaluate to a boolean"
           (lambda () (top-interp '(if 1 2 3))))
(check-exn #px"must evaluate to a function"
           (lambda () (top-interp '(5 1 2))))
(check-exn #px"VEBG"
           (lambda () (top-interp '((fn (x y) -> x) 1))))
(check-exn #px"unbound identifier"
           (lambda () (top-interp 'undefined-var)))