#lang plai-typed

;;;;;;;;;;;;;; TIPOS ;;;;;;;;;;;;;;;
(define-type ExprC
             [numC  (n : number)]
             [idC   (s : symbol)]
             [appC  (fun : symbol) (arg : ExprC)]
             [plusC (l : ExprC) (r : ExprC)]
             [multC (l : ExprC) (r : ExprC)]
             [ifC   (c : ExprC) (s : ExprC) (n : ExprC)])

(define-type ExprS
             [numS  (n : number)]
             [idS   (s : symbol)]
             [appS  (fun : symbol) (arg : ExprS)]
             [plusS (l : ExprS) (r : ExprS)]
             [bminusS (l : ExprS) (r : ExprS)]
             [uminusS (e : ExprS)]
             [multS (l : ExprS) (r : ExprS)]
             [ifS   (c : ExprS) (s : ExprS) (n : ExprS)])

(define-type FunDefC
             [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
    [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

;;;;;;;;;;;;;; PARSER ;;;;;;;;;;;;;;;
(define (parse [s : s-expression]) : ExprS 
  (cond
	[(s-exp-number? s) (numS (s-exp->number s))]
	[(s-exp-list? s)
	 (let ([sl (s-exp->list s)])
	   (case (s-exp->symbol (first sl))
		 [(+) (plusS   (parse (second sl)) (parse (third sl)))]
		 [(-) (case (length sl)
				 [(2)  (uminusS (parse (second sl)))]
				 [else (bminusS (parse (second sl)) (parse (third sl)))])]
		 [(*) (multS   (parse (second sl)) (parse (third sl)))]
		 [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(call) (appS (s-exp->symbol (second sl)) (parse (third sl)))]
		 [else (error 'parse "invalid_list_input")]))]
	[else (error 'parse "invalid_input")]))

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
			 [numS    (n)   (numC n)]
             [idS     (s)   (idC s)]
             [appS    (fun arg) (appC fun (desugar arg))]
			 [plusS   (l r) (plusC (desugar l) (desugar r))]
			 [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
			 [uminusS (e)   (multC (numC -1) (desugar e))]
			 [multS   (l r) (multC (desugar l) (desugar r))]
			 [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]))

;;;;;;;;;;;;;; INTERPRETER ;;;;;;;;;;;;;;;
(define (interp [a : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
			 [numC (n) n]
             [appC (f a) (local ([define fd (get-fundef f fds)])
                            (interp (fdC-body fd)
                                    (extend-env
                                        (bind (fdC-arg fd) (interp a env fds)) env)
                                    fds))]
             [idC   (n)  (lookup n env)]
			 [plusC (l r) (+ (interp l env fds) (interp r env fds))]
			 [multC (l r) (* (interp l env fds) (interp r env fds))]
			 [ifC (c s n) (if (zero? (interp c env fds)) (interp n env fds) (interp s env fds))]))

;;;;;;;;;;;;;; FUNCTIONS ;;;;;;;;;;;;;;;
(define (lookup [for : symbol] [env : Env]) : number
    (cond
        [(empty? env) (error 'lookup "name not found")]
        [else (cond
                [(symbol=? for (bind-name (first env)))
                    (bind-val (first env))]
                [else (lookup for (rest env))])]))

(define (get-fundef [name : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "lista de funções vazia")]
    [(cons? fds)  (cond
                    [(equal? name (fdC-name (first fds))) (first fds)]
                    [else (get-fundef name (rest fds))])]))

;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;
;Soma somente dois numeros
(test (interp (desugar (parse '(+ 2 3 5))) (list) (list)) 5)

;Testa operações
(test (interp (desugar (parse '(+ 3 7))) (list) (list)) 10)
(test (interp (desugar (parse '(- 3 7))) (list) (list)) -4)
(test (interp (desugar (parse '(* 3 7))) (list) (list)) 21)
(test (interp (desugar (parse '(* (+ 3 3) (- 7 2)))) (list) (list)) 30)
(test (interp (desugar (parse '(- 2))) (list) (list)) -2)
(test (interp (desugar (parse '(if (- 2 2) 1 0))) (list) (list)) 0)
(test (interp (desugar (parse '(if (- 2 3) 1 0))) (list) (list)) 1)

;Testa parser
(test (parse '(+ 2 3)) (plusS (numS 2) (numS 3)))

;Testa funções
(define testList 
    (list
        [fdC 'dobro 'x (plusC (idC 'x) (idC 'x))]
        [fdC 'quadrado 'y (multC (idC 'y) (idC 'y))]))

(test (interp (desugar (parse '(call dobro 7))) (list) testList) 14)
(test (interp (desugar (parse '(+ (call quadrado 7) (call dobro 7)))) (list) testList) 63)
(test (interp (desugar (parse '(call quadrado (call quadrado (call dobro 1))))) (list) testList) 16)
