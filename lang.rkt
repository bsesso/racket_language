#lang plai-typed

(define-type ArithC
			 [numC  (n : number)]
			 [plusC (l : ArithC) (r : ArithC)]
			 [multC (l : ArithC) (r : ArithC)]
			 [ifC   (c : ArithC) (s : ArithC) (n : ArithC)])

(define-type ArithS
			 [numS    (n : number)]
			 [plusS   (l : ArithS) (r : ArithS)]
			 [bminusS (l : ArithS) (r : ArithS)]
			 [uminusS (e : ArithS)]
			 [multS   (l : ArithS) (r : ArithS)]
			 [ifS     (c : ArithS) (s : ArithS) (n : ArithS)])

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
			 [numS    (n)   (numC n)]
			 [plusS   (l r) (plusC (desugar l) (desugar r))]
			 [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
			 [uminusS (e)   (multC (numC -1) (desugar e))]
			 [multS   (l r) (multC (desugar l) (desugar r))]
			 [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]))

(define (parse [s : s-expression]) : ArithS
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
		 [else (error 'parse "invalid_list_input")]))]
	[else (error 'parse "invalid_input")]))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
			 [numC (n) n]
			 [plusC (l r) (+ (interp l) (interp r))]
			 [multC (l r) (* (interp l) (interp r))]
			 [ifC (c s n) (if (zero? (interp c)) (interp n) (interp s))]))


;Soma somente dois numeros
(test (interp (desugar (parse '(+ 2 3 5)))) 5)

;Testa operações
(test (interp (desugar (parse '(+ 3 7)))) 10)
(test (interp (desugar (parse '(- 3 7)))) -4)
(test (interp (desugar (parse '(* 3 7)))) 21)
(test (interp (desugar (parse '(* (+ 3 3) (- 7 2))))) 30)
(test (interp (desugar (parse '(- 2)))) -2)
(test (interp (desugar (parse '(if (- 2 2) 1 0)))) 0)
(test (interp (desugar (parse '(if (- 2 3) 1 0)))) 1)

;Testa parser
(test (parse '(+ 2 3)) (plusS (numS 2) (numS 3)))
