#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren base)
        (chrKanren state)
        (chrKanren interp)
        (chrKanren prelude lists)
        (srfi :39 parameters))

(define-relation (evalo env exp val)
  (conde
   [(numbero exp) (== val exp)]
   [(symbolo exp) (lookupo exp env val)]
   [(fresh (x b)
      (== exp `(λ (,x) ,b))
      (symbolo x)
      (== val `(clos ,env ,x ,b)))]
   [(fresh (op e_arg clos x body v_arg)
      (== exp `(,op ,e_arg))
      (evalo env op `(clos ,clos ,x ,body))
      (evalo env e_arg v_arg)
      (evalo (cons (cons x v_arg) clos) body val))]))

(define-relation (typeo env exp typ)
  (conde
   [(numbero exp) (== typ 'number)]
   [(symbolo exp) (lookupo exp env typ)]
   [(fresh (x b in out)
      (== exp `(λ (,x) ,b))
      (symbolo x)
      (== typ `(-> ,in ,out))
      (typeo (cons (cons x in) env) b out))]
   [(fresh (op arg in)
      (== exp `(,op ,arg))
      (typeo env op `(-> ,in ,typ))
      (typeo env arg in))]))

(define-constraint (type^o env exp typ))

(define-rules
  (forall (nv n ty)
    (forget (type^o nv n ty))
    (numbero n)
    <=>
    (== ty 'number))
  (forall (nv n ty)
    (forget (type^o nv n ty))
    (ground number? n)
    <=>
    (== ty 'number))
  (forall (nv rator arglist body ty rst)
    (forget (type^o nv `(,rator ,arglist ,body . ,rst) ty))
    <=>
    (== rst '())
    (fresh (x i o)
      (== rator 'λ)
      (== arglist (list x))
      (symbolo x)
      (== ty `(-> ,i ,o))
      (type^o (cons (cons x i) nv) body o)))
  (forall (nv rator rand typ)
    (forget (type^o nv `(,rator ,rand) typ))
    <=>
    (fresh (t_input)
      (type^o nv rator `(-> ,t_input ,typ))
      (type^o nv rand t_input)))
  (forall (nv sym typ)
    (symbolo sym)
    (forget (type^o nv sym typ))
    <=>
    (lookupo sym nv typ))
  (forall (nv sym typ)
    (forget (type^o nv sym typ))
    (ground symbol? sym)
    <=>
    (lookupo sym nv typ)))

(define-constraint (eval^o env exp val))

(define-rules
  (forall (exp env val)
    (forget (eval^o env exp val)) (ground number? exp) <=> (== exp val))
  (forall (env val nm body)
    (forget (eval^o env `(λ (,nm) ,body) val))
    <=>
    (symbolo nm)
    (== val `(clos ,env ,nm ,body)))
  (forall (op arg env val)
    (forget (eval^o env `(,op ,arg) val))
    <=>
    (fresh (env^ nm body argval)
      (eval^o env   op `(clos ,env^ ,nm ,body))
      (eval^o env  arg argval)
      (eval^o (cons (cons nm argval) env^) body val)))
  (forall (exp env val)
    (forget (eval^o env exp val))
    (ground symbol? exp)
    <=>
    (lookupo exp env val)))

(define M `(λ (x) (x x)))
(define Ω `(,M ,M))
