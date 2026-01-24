#!r6rs

(library (chrKanren prelude eval)
  (export eval^o evalo)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren base)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren rule)
          (chrKanren prelude lists)
          (chrKanren prelude unification)
          (chrKanren prelude types))

  (define-constraint (eval^o obj env res))

  (define-relation (evalo tm nv rs)
    (conde
     [(numbero tm) (== tm rs)]
     [(== tm `(quote ,rs))]
     [(symbolo tm) (lookupo tm nv rs)]
     [(fresh (a d ra rd)
        (== tm `(cons ,a ,d))
        (== rs (cons ra rd))
        (evalo a nv ra)
        (evalo d nv rd))]
     [(fresh (al bd)
        (== tm `(lambda ,al ,bd))
        (listo al)
        (== rs `(closure ,al ,bd ,nv)))]
     [(fresh (rator rands)
        (== tm (cons rator rands))
        (listo rands)
        (fresh (al bd nv^ nv^^)
          (evalo rator nv `(closure ,al ,bd ,nv^))
          (eval-argso al rands nv nv^ nv^^)
          (evalo bd nv^^ rs)))]))

  (define (eval_-argso eval_o)
    (define-relation (loop-argso arglist
                                 operands
                                 eval-env
                                 base-env
                                 res-env)
      (conde
       [(== arglist '())
        (== operands '())
        (== base-env res-env)]
       [(fresh (a as r o os)
          (== arglist (cons a as))
          (== operands (cons o os))
          (callo eval_o o eval-env r)
          (loop-argso as
                      os
                      eval-env
                      (cons (cons a r) base-env)
                      res-env))]))
    loop-argso)

  (define eval-argso  (eval_-argso evalo))
  (define eval^-argso (eval_-argso eval^o))

  (define-rules
    (forall (nb nv rs)
      (eval^o nb nv rs)
      (ground number? nb)
      <=>
      (== nb rs))
    (forall (nb nv rs)
      (eval^o nb nv rs)
      (numbero nb)
      <=>
      (== nb rs))
    (forall (vl nv rs)
      (eval^o `(quote ,vl) nv rs)
      <=>
      (== vl rs))
    (forall (ar dr nv rs)
      (eval^o `(cons ,ar ,dr) nv rs)
      <=>
      (fresh (h t)
        (== rs (cons h t))
        (eval^o ar nv h)
        (eval^o dr nv t)))
    (forall (arglist nv body rs)
      (eval^o `(lambda ,arglist ,body) nv rs)
      <=>
      (listo arglist)
      (== rs `(closure ,arglist ,body ,nv)))
    (forall (rator rands nv rs)
      (eval^o `(,rator . ,rands) nv rs)
      <=>
      (listo rands)
      (fresh (arglist body env env^)
        (eval^o rator nv `(closure ,arglist ,body ,env))
        (eval^-argso arglist rands nv env env^)
        (eval^o body env^ rs)))
    (forall (nm nv rv)
      (eval^o nm nv rv)
      (symbolo nm)
      <=>
      (lookupo nm nv rv))
    (forall (nm nv rv)
      (eval^o nm nv rv)
      (ground symbol? nm)
      <=>
      (lookupo nm nv rv))))
