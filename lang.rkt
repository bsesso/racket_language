#lang plai-typed

;;;;;;;;;;;;;; TIPOS ;;;;;;;;;;;;;;;
(define-type ExprC
             [numC  (n : number)]
             [idC   (s : symbol)]
             [plusC (l : ExprC) (r : ExprC)]
             [multC (l : ExprC) (r : ExprC)]
             [fdC   (name : symbol) (arg : symbol) (body : ExprC)]
             [appC  (fun : ExprC) (arg : ExprC)]
             [ifC   (c : ExprC) (s : ExprC) (n : ExprC)])

(define-type ExprS
             [numS  (n : number)]
             [idS   (s : symbol)]
             [fdS   (name : symbol) (arg : symbol) (body : ExprS)]
             [appS  (fun : ExprS) (arg : ExprS)]
             [plusS (l : ExprS) (r : ExprS)]
             [bminusS (l : ExprS) (r : ExprS)]
             [uminusS (e : ExprS)]
             [multS (l : ExprS) (r : ExprS)]
             [ifS   (c : ExprS) (s : ExprS) (n : ExprS)])

(define-type Binding
    [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

(define-type Value
    [numV (n : number)]
    [funV (name : symbol) (arg : symbol) (body : ExprC)])

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
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
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
         [(func) (fdS (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]
		 [else (error 'parse "invalid_list_input")]))]
	[else (error 'parse "invalid_input")]))

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
			 [numS    (n)   (numC n)]
             [idS     (s)   (idC s)]
             [appS    (fun arg) (appC (desugar fun) (desugar arg))]
             [fdS     (n a b) (fdC n a (desugar b))]
			 [plusS   (l r) (plusC (desugar l) (desugar r))]
			 [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
			 [uminusS (e)   (multC (numC -1) (desugar e))]
			 [multS   (l r) (multC (desugar l) (desugar r))]
			 [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]))

;;;;;;;;;;;;;; INTERPRETER ;;;;;;;;;;;;;;;
(define (interp [a : ExprC] [env : Env]): Value
  (type-case ExprC a
			 [numC (n) (numV n)]
             [appC (f a) (local ([define fd (interp f env)])
                            (interp (funV-body fd)
                                    (extend-env
                                        (bind (funV-arg fd) (interp a env)) mt-env)))]
             [idC   (n)  (lookup n env)]
             [fdC (n a b) (funV n a b)]
			 [plusC (l r) (num+ (interp l env) (interp r env))]
			 [multC (l r) (num* (interp l env) (interp r env))]
			 [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]))

;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;
(define (lookup [for : symbol] [env : Env]) : Value
    (cond
        [(empty? env) (error 'lookup "name not found")]
        [else (cond
                [(symbol=? for (bind-name (first env)))
                    (bind-val (first env))]
                [else (lookup for (rest env))])]))

;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;
;Soma somente dois numeros
(test (interp (desugar (parse '(+ 2 3 5))) (list)) (numV 5))

;Testa operações
(test (interp (desugar (parse '(+ 3 7))) (list)) (numV 10))
(test (interp (desugar (parse '(- 3 7))) (list)) (numV -4))
(test (interp (desugar (parse '(* 3 7))) (list)) (numV 21))
(test (interp (desugar (parse '(* (+ 3 3) (- 7 2)))) (list)) (numV 30))
(test (interp (desugar (parse '(- 2))) (list)) (numV -2))
(test (interp (desugar (parse '(if (- 2 2) 1 0))) (list)) (numV 0))
(test (interp (desugar (parse '(if (- 2 3) 1 0))) (list)) (numV 1))

;Testa parser
(test (parse '(+ 2 3)) (plusS (numS 2) (numS 3)))

;Testa funções
(test (interp (desugar (parse '(call [func dobro x (+ x x)] 7))) (list)) (numV 14))
(test (interp (desugar (parse '(+ (call [func quadrado x (* x x)] 7) (call [func dobro x (+ x x)] 7)))) (list)) (numV 63))
(test (interp (desugar (parse '(call [func quadrado x (* x x)] (call [func quadrado y (* y y)] (call [func dobro z (+ z z)] 1))))) (list)) (numV 16))
