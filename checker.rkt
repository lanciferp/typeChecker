#lang plait
(require (typed-in "parser.rkt" [

(define (typecheck [expr : Expr])
  (type-case Expr expr
    [(numE n) (numT)]
    [(plusE l r) 
     (let ([lt (typecheck l)] 
           [rt (typecheck r)])
       (if (and (equal? lt (numT))
                (equal? rt (numT)))
         (numT)
         (error 'e "Not Both Numbers")))]
    [(equalE l r) 
     (let ([lt (typecheck l)] 
           [rt (typecheck r)])
       (if (and (equal? lt (numT))
                (equal? rt (numT)))
         (boolT)
         (error 'e "Not Both Numbers")))]
   [(notE a) 
     (let ([at (typecheck a)]) 
       (if (equal? at (boolT))
         (boolT)
         (error 'e "Not Both Bools")))]))

(define (lookup gamma x) (gamma x))

(define (extend gamma x t)
  (λ (y) (if (equal? x y) t (lookup gamma y))))

(define-type Type
  (numT)
  (arrowT [domain : Type] [range : Type]))

(define-type Expr
  ....
  (recE [f : Symbol] [x : Symbol] [t-in : Type] [t-out : Type] [e : Expr])
  ....)

(define (tc gamma e)
  (type-case Expr e
    ....
    [(recE f x t-in t-out e)
     (let ([extended-e (extend gamma x t-in)])
       (cond
        [(not (equal? t-out (tc
                                (extend extended-e x t-out) e)))
              (error 'tc "body return type not correct")]
        [else (arrowT t-in t-out)]))] ; fill in here (and only here)
    ....))