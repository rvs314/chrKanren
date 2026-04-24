#!r6rs

(import (rnrs)
        (chrKanren base)
        (chrKanren prelude lists))


(define-relation (evalo exp env val)
  (conde
   [(numbero exp) (== val exp)]
   [(symbolo exp) (lookupo exp env val)]
   [(fresh (aex dex aval dval)
      (== exp `(cons ,aex ,dex))
      (== val (cons aval dval))
      (evalo aex env aval)
      (evalo dex env dval))]
   [(fresh (x b)
      (== exp `(lambda (,x) ,b))
      (symbolo x)
      (== val `(clos ,env ,x ,b)))]
   [(fresh (op e_arg clos x body v_arg)
      (== exp `(,op ,e_arg))
      (evalo op env `(clos ,clos ,x ,body))
      (evalo e_arg env v_arg)
      (evalo body (cons (cons x v_arg) clos) val))]))

(define-constraint (eval^o exp env val))

(define-rules
  (forall (exp env val)
    (forget (eval^o exp env val)) (ground number? exp) => (== exp val))
  (forall (env val nm body)
    (forget (eval^o `(lambda (,nm) ,body) env val))
    =>
    (symbolo nm)
    (== val `(clos ,env ,nm ,body)))
  (forall (op arg env val)
    (forget (eval^o `(,op ,arg) env val))
    =>
    (fresh (env^ nm body argval)
      (eval^o op   env `(clos ,env^ ,nm ,body))
      (eval^o arg  env argval)
      (eval^o body (cons (cons nm argval) env^) val)))
  (forall (exp env val)
    (forget (eval^o exp env val))
    (ground symbol? exp)
    =>
    (lookupo exp env val))
  (forall (lhs rhs env val)
    (forget (eval^o `(cons ,lhs ,rhs) env val))
    =>
    (fresh (lval rval)
      (== val (cons lval rval))
      (eval^o lhs env lval)
      (eval^o rhs env rval))))

(define I '(λ (x) x))
(define K '(λ (x) (λ (y) x)))
(define S '(λ (f) (λ (g) (λ (x) ((f x) (g x))))))

(define-relation (bound-variableo var env)
  (fresh (k v env-rst)
    (== env (cons (cons k v) env-rst))
    (conde
     [(== k var)]
     [(bound-variableo var env-rst)])))

#;
(define-relation (termo t env)
  (conde
   [(numbero t)]
   [(bound-variableo t env)]
   [(fresh ())]))

(define-syntax lam
  (syntax-rules ()
    [(_ ()         b) b]
    [(_ (x ys ...) b) `(λ (x) ,(let ([x 'x])
                                 (lam (ys ...) b)))]))

(define-syntax app
  (syntax-rules ()
    [(_ a)         a]
    [(_ a b c ...) (app `(,a ,b) c ...)]))

(define rs
  (run 1 (q)
    (eval^o `((,q 1) 2) '() 1)
    (evalo  `((,q 3) 4) '() 3)))
