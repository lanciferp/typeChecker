#lang plait

(require (typed-in "checker.rkt" [type-of : (Expr -> Type) ]))

(define-type Expr
  (num [n : Number])
  (id [x : Symbol])
  (bool [b : Boolean])
  (bin-num-op [op : (Number Number -> Number)] [lhs : Expr] [rhs : Expr])
  (iszero [e : Expr])
  (bif [test : Expr] [then : Expr] [else : Expr])
  (with [bound-x : Symbol] [bound-body : Expr] [body : Expr])
  (fun [arg-x : Symbol]
       [arg-type : Type]
       [body : Expr]
       [body-type : Type])
  (app [fun : Expr] [arg : Expr])
  (nempty)
  (ncons [first : Expr] [rest : Expr])
  (nfirst [e : Expr])
  (nrest [e : Expr])
  (isnempty [e : Expr]))

(define-type Type
  (t-num)
  (t-bool)
  (t-nlist)
  (t-fun [arg : Type] [result : Type]))

(parse : (S-Exp -> Expr))
(parse-type : (S-Exp -> Type))

(define (parse-type [s : S-Exp]) : Type
  (cond
    [(s-exp-list? s) (let ([sl (s-exp->list s)])
                       (t-fun (parse-type (first sl)) (parse-type (third sl))))]
    [(s-exp-match? `number s) (t-num)]
    [(s-exp-match? `boolean s) (t-bool)]
    [(s-exp-match? `nlist s) (t-nlist)]
    ))

(define (parse [s : S-Exp]) : Expr
  (cond
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(s-exp-symbol? (first sl))
                         (let ([sym (s-exp->symbol (first sl))])
                           (cond
                             [(eq? sym '+) (bin-num-op + (parse (second sl)) (parse (third sl)))]
                             [(eq? sym '*) (bin-num-op * (parse (second sl)) (parse (third sl)))]
                             [(eq? sym '-) (bin-num-op - (parse (second sl)) (parse (third sl)))]
                             [(eq? sym 'zero?) (iszero (parse (second sl)))]
                             [(eq? sym 'if) (bif (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                             [(eq? sym 'cons) (ncons (parse (second sl)) (parse (third sl)))]
                             [(eq? sym 'first) (nfirst (parse (second sl)))]
                             [(eq? sym 'rest) (nrest (parse (second sl)))]
                             [(eq? sym 'empty?) (isnempty (parse  (second sl)))]
                             [(eq? sym 'with) (let ([wl (s-exp->list (second sl))])
                                                (with (s-exp->symbol (first wl)) (parse (second wl)) (parse (third sl))))]
                             [(eq? sym 'fun) (let ([fl (s-exp->list (second sl))])
                                               (fun (s-exp->symbol (first fl)) (parse-type (third fl))
                                                    (parse (list-ref sl 4)) (parse-type (fourth sl))))]  
                             [else (error 'parse "invalid list input")]))]
         [else (app (parse (first sl)) (parse (second sl)))]))]
    [(s-exp-match? `NUMBER s) (num (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (let ([sym (s-exp->symbol s)])
       (cond
         [(eq? sym 'true) (bool #true)]
         [(eq? sym 'false) (bool #false)]
         [(eq? sym 'empty) (nempty)]
         [else (id sym)]))]
    [else (nempty)]))

(parse `(fun (g : number) : (number -> number) (+ g 3)))

(define (lookup gamma x) (gamma x))

(define (extend gamma x t)
  (λ (y) (if (equal? x y) t (lookup gamma y))))

(define (typecheck [gamma : (Symbol -> Type)]  [expr : Expr])
  (type-case Expr expr
    [(num n) (t-num)]
    [(id x) (lookup gamma x)]
    [(bool b) (t-bool)]
    [(bin-num-op op l r)
     (let ([lt (typecheck gamma l)]
           [rt (typecheck gamma r)])
       (if (and (equal? lt (t-num))
                (equal? rt (t-num)))
         (t-num)
         (error 'e "not both numbers")))]
    [(iszero e) (let ([et (typecheck gamma e)])
                  (type-case Type et
                    [(t-num) (t-bool)]
                    [else (error 'type-error "NaN")]))]
    [(bif if t e) (let ([ift (typecheck gamma if)]
                        [tt (typecheck gamma t)]
                        [et (typecheck gamma e)])
                    (type-case Type ift
                      [(t-bool) (cond
                                  [(equal? tt et) tt]
                                  [else (error 'type-error "then and else clauses don't match types")])]
                      [else (error 'type-error "test clause invalid")]))]
    [(with bx bb b) (letrec ([bbt (typecheck gamma bb)]
                          [e-gamma (extend gamma bx bbt)])
                      (typecheck e-gamma b))]
    [(fun ax at b bt) (letrec ([e-gamma (extend gamma ax at)]
                               [abt (typecheck e-gamma b)])
                        (cond
                          [(equal? bt abt) (t-fun at bt)]
                          [else (error 'type-error "wrong return type")]))]
    [(app f a) (let ([at (typecheck gamma a)]
                     [ft (typecheck gamma f)])
                 [type-case Type ft
                   [(t-fun fat frt) (if (equal? fat at) ft (error 'type-error "arg type doesn't match fun"))]
                   (else (error 'type-error "not a function"))])]
    [(nempty) (t-nlist)]
    [(ncons f r)
     (let ([ft (typecheck gamma f)]
           [rt (typecheck gamma r)])
       (type-case Type ft
          [(t-num) (type-case Type rt
                     [(t-nlist) (t-nlist)]
                     [else (error 'type-error "not a list")])]
          [else (error 'type-error "lists can only have numbers")]))]
    [(nfirst e) (let ([et (typecheck gamma e)])
                    (type-case Type et
                      [(t-nlist) (t-num)]
                      [else (error 'type-error "not a list")]
                      ))]
    [(nrest e) (let ([et (typecheck gamma e)])
                    (type-case Type et
                      [(t-nlist) (t-nlist)]
                      [else (error 'type-error "not a list")]
                      ))]
    [(isnempty e) (let ([et (typecheck gamma e)])
                    (type-case Type et
                      [(t-nlist) (t-bool)]
                      [else (error 'type-error "not a list")]
                      ))]
    ))

(define (mt-gamma [/ : Symbol]) : Type
  (error 'ubound-id "UNBOUND"))

(define (type-of [expr : Expr]) : Type
  (typecheck mt-gamma expr))

; parse tests
(test (parse `5) (num 5))
(test (parse `empty) (nempty))
(test (parse `true) (bool #t))
(test (parse `(empty? (cons 1 empty))) (isnempty (ncons (num 1) (nempty))))
(test (parse `(+ 5 6)) (bin-num-op + (num 5) (num 6)))
(test (parse `(fun (g : number) : (number -> number) (+ g 3)))
      (fun 'g (t-num) (bin-num-op + (id 'g) (num 3)) (t-fun (t-num) (t-num))))

; basic types tests
(test (type-of (parse `0)) (t-num))
(test (type-of (parse `true)) (t-bool))
(test (type-of (parse ` (cons 1 empty))) (t-nlist))
(test/exn (type-of (parse `a)) "")

; bin-num-op tests
(test (type-of (parse `(+ 1 2))) (t-num))
(test (type-of (parse `(* 3 4))) (t-num))
(test/exn (type-of (parse `(+ 4 h))) "")
(test/exn (type-of (parse `(/ 7 0))) "")

; iszero tests
(test (type-of (parse `(zero? 0))) (t-bool))
(test (type-of (parse `(zero? (first (cons 1 empty))))) (t-bool))
(test/exn (type-of (parse `(zero? true))) "")
(test/exn (type-of (parse `(zero? (cons 1 empty)))) "")

; if tests
(test (type-of (parse `(if (zero? 0) 1 2))) (t-num))
(test (type-of (parse `(if (empty? (cons 2 empty)) (+ 4 5) (* 3 2)))) (t-num))
(test/exn (type-of (parse `(if 1 1 1))) "")
(test/exn (type-of (parse `(if true 1 false))) "")
(test/exn (type-of (parse `(if (zero? (- 1 9)) 1 a))) "")

; with tests
(test (type-of (parse `(with (x 4) (+ x 1)))) (t-num))
(test (type-of (parse `(with (x 4) (with (y 4) (+ x y))))) (t-num))
(test/exn (type-of (parse `(with (4 4) (4 4 4)))) "")
(test/exn (type-of (parse  `(with (x y) (+ x y)))) "")

; oxymoronic tests
(test (type-of (parse `(fun (x : number) : number (+ x 4)))) (t-fun (t-num) (t-num)))
(test (type-of (parse `(fun (x : number) : (number -> number) (fun (y : number) : number (+ x y)))))
      (t-fun (t-num) (t-fun (t-num) (t-num))))

; app tests
(test (type-of (parse `((fun (x : number) : number (+ x 4)) 2))) (t-fun (t-num) (t-num)))
(test/exn (type-of (parse `(3 4)))  "")

; empty tests
(test (type-of (parse `empty)) (t-nlist))
(test (type-of (parse `(cons 1 empty))) (t-nlist))

; cons tests
(test (type-of (parse `(cons 1 empty))) (t-nlist))
(test/exn (type-of (parse `(cons 1 1))) "")

; first tests
(test (type-of (parse `(first (cons 1 empty)))) (t-num))
(test (type-of (parse `(first (cons 2 (cons 3 empty))))) (t-num))
(test (type-of (parse `(first (rest (cons 1 (cons 2 empty)))))) (t-num))
(test (type-of (parse `(first empty))) (t-num)) ;runtime error
(test/exn (type-of (parse `(first 1))) "");
(test/exn (type-of (parse `(first a))) "")

; rest tests
(test (type-of (parse `(rest (cons 1 (cons 2 empty))))) (t-nlist))
(test (type-of (parse `(rest (rest (cons 2 (cons 3 (cons 6 empty))))))) (t-nlist))
(test (type-of (parse `(rest empty))) (t-nlist)) ;runtime error                   

; empty? tests
(test (type-of (parse `(empty? empty))) (t-bool))
(test (type-of (parse `(empty? (cons 1 empty)))) (t-bool))
(test/exn (type-of (parse `(empty? 1))) "")
(test/exn (type-of (parse `(empty? (cons true empty)))) "")
(test/exn (type-of (parse `(empty? (cons 1 a)))) "")