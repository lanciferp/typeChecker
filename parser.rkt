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

(parse : (S-Exp -> Expr))

(define (parse [s : S-Exp]) : Expr
  (cond
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (let ([sym (s-exp->symbol (first sl))])
         (cond
           [(eq? sym '+) (bin-num-op + (parse (second sl)) (parse (third sl)))]
           [(eq? sym '*) (bin-num-op * (parse (second sl)) (parse (third sl)))]
           [(eq? sym '-) (bin-num-op - (parse (second sl)) (parse (third sl)))]
           [(eq? sym 'zero?) (iszero (parse (second sl)))]
           [(eq? sym 'if) (bif (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
           [(eq? sym 'cons) (ncons (parse (second sl)) (parse (third sl)))]
           [
           [(eq? sym 'with) (let ([wl (s-exp->list (second sl))])
                                  (with (s-exp->symbol (first wl)) (parse (second wl)) (parse (third sl))))]
           [(eq? sym 'empty?) (isnempty (parse  (second sl)))]
           [(eq? sym 'first) (nfirst (parse (second sl)))]
           [else (error 'parse "invalid list input")])))]
       
    [(s-exp-match? `NUMBER s) (num (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (let ([sym (s-exp->symbol s)])
       (cond
         [(eq? sym 'true) (bool #true)]
         [(eq? sym 'false) (bool #false)]
         [(eq? sym 'empty) (nempty)]
    ))]
    [else (nempty)]
 ))
  
(parse `(+ 5 6))



