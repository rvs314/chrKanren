#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren base)
        (chrKanren state)
        (chrKanren interp)
        #;(only (racket) time-apply)
        (chrKanren prelude lists)
        (srfi :39 parameters))

(define-relation (evalo env exp val)
  (conde
   [(numbero exp) (=== val exp)]
   [(symbolo exp) (lookupo exp env val)]
   [(fresh (x b)
      (=== exp `(λ (,x) ,b))
      (symbolo x)
      (=== val `(clos ,env ,x ,b)))]
   [(fresh (op e_arg clos x body v_arg)
      (=== exp `(,op ,e_arg))
      (evalo env op `(clos ,clos ,x ,body))
      (evalo env e_arg v_arg)
      (evalo (cons (cons x v_arg) clos) body val))]))

(define-relation (typeo env exp typ)
  (conde
   [(numbero exp) (=== typ 'number)]
   [(symbolo exp) (lookupo exp env typ)]
   [(fresh (x b in out)
      (=== exp `(λ (,x) ,b))
      (symbolo x)
      (=== typ `(-> ,in ,out))
      (typeo (cons (cons x in) env) b out))]
   [(fresh (op arg in)
      (=== exp `(,op ,arg))
      (typeo env op `(-> ,in ,typ))
      (typeo env arg in))]))

(define-constraint (type^o env exp typ))

(define-rules
  (forall (nv n ty)
    (type^o nv n ty)
    (numbero n)
    <=>
    (numbero n)
    (=== ty 'number))
  (forall (nv n ty)
    (type^o nv n ty)
    (ground number? n)
    <=>
    (=== ty 'number))
  (forall (nv rator arglist body ty rst)
    (type^o nv `(,rator ,arglist ,body . ,rst) ty)
    <=>
    (=== rst '())
    (fresh (x i o)
      (=== rator 'λ)
      (=== arglist (list x))
      (symbolo x)
      (=== ty `(-> ,i ,o))
      (type^o (cons (cons x i) nv) body o)))
  (forall (nv rator rand typ)
    (type^o nv `(,rator ,rand) typ)
    <=>
    (fresh (t_input)
      (type^o nv rator `(-> ,t_input ,typ))
      (type^o nv rand t_input)))
  (forall (nv sym typ)
    (symbolo sym)
    (type^o nv sym typ)
    <=>
    (symbolo sym)
    (lookupo sym nv typ))
  (forall (nv sym typ)
    (type^o nv sym typ)
    (ground symbol? sym)
    <=>
    (lookupo sym nv typ)))

#;
(time-apply
 (lambda ()
   (run 1 (p v t)
     (typeo '() p t)
     (evalo '() `(,p 1) 1)
     (evalo '() `(,p 2) 2)
     (evalo '() `(,p 3) 3)))
 '())

#;(define (iterate fn count start)
    (if (zero? count)
        '()
        (let ([next (fn start)])
          (cons next (iterate fn (- count 1) next)))))

#;(define k (fresh (p v) (type^o '() p 'number) (evalo '() `(,p 1) v)))
#;(define series (iterate step 43 (start empty-state k)))

#;
(run 1 (p v)
  (type^o '() p 'number)
  (fresh (clos x body)
    (conde
     [(numbero p) (=== `(clos ,clos ,x ,body) p)]
     [(symbolo p) (lookupo p '() `(clos ,clos ,x ,body))]
     [(fresh (x b)
        (=== p `(λ (,x) ,b))
        (symbolo x)
        (=== `(clos ,clos ,x ,body) `(clos ,'() ,x ,b)))]
     [(fresh (op e_arg clos x body v_arg)
        (=== p `(,op ,e_arg))
        (evalo '() op `(clos ,clos ,x ,body))
        (evalo '() e_arg v_arg)
        (evalo (cons (cons x v_arg) clos) body `(clos ,clos ,x ,body)))])
    (evalo (cons (cons x 1) clos) body v)))

(define S
  '(λ (f)
     (λ (g)
       (λ (x)
         ((f x) (g x))))))
(define K '(λ (x) (λ (y) x)))
(define I '(λ (x) x))
(define )

(define S-type
  '((((-> (-> number (-> number number))
          (-> (-> number number)
              (-> number
                  number)))))))

(define (app . xs)
  (cond
    [(null? xs) (error 'app "Cannot apply an empty list")]
    [(singleton? xs) (car xs)]
    [else (list (car xs) (apply app (cdr xs)))]))

(define (S-examples ))
