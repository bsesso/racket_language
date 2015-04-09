#lang plai-typed

;;;;;;;;;;;;;; TIPOS ;;;;;;;;;;;;;;;
(define-type ExprC
             [numC  (n : number)]
             [varC  (s : symbol)]
             [plusC (l : ExprC) (r : ExprC)]
             [multC (l : ExprC) (r : ExprC)]
             [lamC  (arg : symbol) (body : ExprC)]
             [appC  (fun : ExprC) (arg : ExprC)]
             [ifC   (c : ExprC) (s : ExprC) (n : ExprC)]
             [setC  (var : symbol) (arg : ExprC)]
             [seqC  (b1 : ExprC) (b2 : ExprC)])

(define-type ExprS
             [numS  (n : number)]
             [defS  (s : symbol) (v : ExprS) (body : ExprS)]
             [varS  (s : symbol)]
             [lamS  (arg : symbol) (body : ExprS)]
             [appS  (fun : ExprS) (arg : ExprS)]
             [plusS (l : ExprS) (r : ExprS)]
             [bminusS (l : ExprS) (r : ExprS)]
             [uminusS (e : ExprS)]
             [multS (l : ExprS) (r : ExprS)]
             [ifS   (c : ExprS) (s : ExprS) (n : ExprS)]
             [setS  (var : symbol) (arg : ExprS)]
             [seqS  (b1 : ExprS) (b2 : ExprS)])

(define-type Binding
    [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

(define-type Value
    [numV  (n : number)]
    [closV (arg : symbol) (body : ExprC) (env : Env)])

(define-type-alias Location number)

(define-type Storage
             [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))

(define-type Result
             [v*s (v : Value) (s : Store)])

(define mt-store empty)

(define override-store cons)

(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r)) (numV (+ (numV-n l) (numV-n r)))]
        [else (error 'num+ "Um dos argumentos não é inteiro")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? l)) (numV (* (numV-n l) (numV-n r)))]
        [else (error 'num* "Um dos argumentos não é inteiro")]))


;;;;;;;;;;;;;; PARSER ;;;;;;;;;;;;;;;
(define (parse [s : s-expression]) : ExprS 
  (cond
	[(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (varS (s-exp->symbol s))]
	[(s-exp-list? s)
	 (let ([sl (s-exp->list s)])
	   (case (s-exp->symbol (first sl))
		 [(+) (plusS   (parse (second sl)) (parse (third sl)))]
		 [(-) (case (length sl)
				 [(2)  (uminusS (parse (second sl)))]
				 [else (bminusS (parse (second sl)) (parse (third sl)))])]
		 [(*) (multS   (parse (second sl)) (parse (third sl)))]
		 [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(func) (lamS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(def) (defS (s-exp->symbol (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(set) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
		 [else (error 'parse "invalid_list_input")]))]
	[else (error 'parse "invalid_input")]))

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
			 [numS    (n)   (numC n)]
             [appS    (fun arg) (appC (desugar fun) (desugar arg))]
             [varS    (v)   (varC v)]
             [defS    (s v b) (appC (lamC s (desugar b)) (desugar v))]
             [lamS    (a b) (lamC a (desugar b))]
			 [plusS   (l r) (plusC (desugar l) (desugar r))]
			 [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
			 [uminusS (e)   (multC (numC -1) (desugar e))]
			 [multS   (l r) (multC (desugar l) (desugar r))]
			 [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
             [setS    (v a) (setC v (desugar a))]
             [seqS    (b1 b2) (seqC (desugar b1) (desugar b2))]))

;;;;;;;;;;;;;; INTERPRETER ;;;;;;;;;;;;;;;
(define (interp [a : ExprC] [env : Env] [sto : Store]): Result
  (type-case ExprC a
			 [numC (n) (v*s (numV n) sto)]
             [appC (f a) (let ([res-f (interp f env sto)])
                           (let ([res-a (interp a env (v*s-s res-f))]
                                 [new (new-loc)])
                              (interp (closV-body (v*s-v res-f))
                                   (extend-env (bind (closV-arg (v*s-v res-f)) new)
                                               (closV-env (v*s-v res-f)))
                                   (override-store (cell new (v*s-v res-a)) (v*s-s res-a)))))]
             [varC  (n)  (v*s (fetch (lookup n env) sto) sto)]
             [lamC  (a b) (v*s (closV a b env) sto)]
			 [plusC (l r) (let ([res-l (interp l env sto)])
                                (let ([res-r (interp r env (v*s-s res-l))])
                                    (v*s (num+ (v*s-v res-l) (v*s-v res-r)) (v*s-s res-r))))]
			 [multC (l r) (let ([res-l (interp l env sto)])
                                (let ([res-r (interp r env (v*s-s res-l))])
                                    (v*s (num* (v*s-v res-l) (v*s-v res-r)) (v*s-s res-r))))]
 
			 [ifC (c s n) (let ([res-c (interp c env sto)])
                            (if (zero? (numV-n (v*s-v res-c)))
                              (interp n env (v*s-s res-c))
                              (interp s env (v*s-s res-c))))]
             [setC (v a)  (let ([res-a (interp a env sto)]
                                [loc (lookup v env)])
                            (v*s (v*s-v res-a)
                                 (override-store (cell loc (v*s-v res-a))
                                                 (v*s-s res-a))))]
             [seqC (b1 b2) (let ([s-b1 (v*s-s (interp b1 env sto))])
                             (interp b2 env s-b1))]))

;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;
(define (lookup [for : symbol] [env : Env]) : Location 
    (cond
        [(empty? env) (error 'lookup (string-append (string-append "name \'" (symbol->string for)) "\' not found"))]
        [else (cond
                [(symbol=? for (bind-name (first env)))
                    (bind-val (first env))]
                [else (lookup for (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'fetch "Location not found")]
    [else (cond
            [(= loc (cell-location (first sto)))
                (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (+ 1 (unbox n)))
        (unbox n)))))


;;;;;;;;;;;;;; FACILITADOR ;;;;;;;;;;;;;;; 
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env mt-store))
(define (testS   [s : s-expression] [e : Value]) (test (v*s-v (interpS s)) e))

;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;
;Soma somente dois numeros
(testS '(+ 2 3 5) (numV 5))

;Testa operações
(testS '(+ 3 7) (numV 10))
(testS '(- 3 7) (numV -4))
(testS '(* 3 7) (numV 21))
(testS '(* (+ 3 3) (- 7 2)) (numV 30))
(testS '(- 2) (numV -2))
(testS '(if (- 2 2) 1 0) (numV 0))
(testS '(if (- 2 3) 1 0) (numV 1))

;Testa parser
(test (parse '(+ 2 3)) (plusS (numS 2) (numS 3)))

;Testa funções
(testS '(call [func x (+ x x)] 7) (numV 14))
(testS '(+ (call [func x (* x x)] 7) (call [func x (+ x x)] 7)) (numV 63))
(testS '(call [func x (* x x)] (call [func x (* x x)] (call [func x (+ x x)] 1))) (numV 16))
(testS '(call [func x (+ 2 (call [func x (+ x x)] (* 2 x)))] 2) (numV 10))
(testS '(call [func x (call [func y x] 2)] 3) (numV 3)) 

;Testa var
(interpS '(def x 10 (seq (set x 2) (* 2 x))))
