#lang plait

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

(define (lookup gamma x) (gamma x))

(define (extend gamma x t)
  (λ (y) (if (equal? x y) t (lookup gamma y))))

(define (typecheck [gamma : (Symbol -> Type)]  [expr : Expr])
  (type-case Expr expr
    [(num n) (t-num)]
    [(bin-num-op op l r)
     (let ([lt (typecheck gamma l)]
           [rt (typecheck gamma r)])
       (if (and (equal? lt (t-num))
                (equal? rt (t-num)))
         (t-num)
         (error 'e "Not Both Numbers")))]
    [else (error 'e "NOT DONE")]))
(define (mt-gamma [x : Symbol]) : Type
  (error 'e "UNDOUND"))

(define (type-of [expr : Expr]) : Type
  (typecheck mt-gamma expr)
  )