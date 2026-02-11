#!r6rs

(import (rnrs)
        (only (srfi :1 lists) lset<=)
        (chrKanren utils)
        (chrKanren base)
        (only (chrKanren prelude lists) for-allo)
        (rename (chrKanren prelude lists) [lookupo eager:lookupo] ))

(define-constraint (extend-envo ns is e e^))

(define-rules
  (forall (ns is e)
    (extend-envo ns is e '())
    <=>
    (== ns '())
    (== is '())
    (== e '()))
  (forall (is e e^)
    (extend-envo '() is e e^)
    <=>
    (== is '())
    (== e e^))
  (forall (ns e e^)
    (extend-envo ns '() e e^)
    <=>
    (== ns '())
    (== e e^))
  (forall (ns is e)
    (extend-envo ns is e e)
    <=>
    (== ns is))
  (forall (nns n is e e^)
    (extend-envo (cons n nns) is e e^)
    <=>
    (fresh (i iis)
      (== is (cons i iis))
      (extend-envo nns iis `((,n : ,i) . ,e) e^)))
  (forall (iis i ns e e^)
    (extend-envo ns (cons i iis) e e^)
    <=>
    (fresh (n nns)
      (== ns (cons n nns))
      (extend-envo nns iis `((,n : ,i) . ,e) e^))))

(define-constraint (lookupo k al v))

(define-rules
  (forall (k v)
    (lookupo k '() v)
    <=>
    fail)
  (forall (k v k₁ v₁ r)
    (lookupo k (cons `(,k₁ : ,v₁) r) v)
    <=>
    (conde
     [(== k k₁) (== v v₁)]
     [(=/= k k₁) (lookupo k r v)])))

(define-constraint (⊢o env term type))

(define-relation (⊢*o env terms types)
  (for-allo (lambda (ty tm) (⊢o env ty tm)) terms types))

(define-rules
  (forall (e p t)
    (⊢o e p t)
    (ground integer? p)
    <=>
    (== t 'int))
  (forall (e n t)
    (symbolo n)
    (⊢o e n t)
    <=>
    (symbolo n)
    (lookupo n e t))
  (forall (e n t)
    (⊢o e n t)
    (ground symbol? n)
    <=>
    (lookupo n e t))
  (forall (e ns v t)
    (⊢o e `(λ ,ns ,v) t)
    <=>
    (for-allo symbolo ns)
    (fresh (is o e^)
      (== t `(-> ,is ,o))
      (extend-envo ns is e e^)
      (⊢o e^ v o)))
  (forall (e rator rands t)
    (⊢o e `(app ,rator . ,rands) t)
    <=>
    (fresh (i)
      (⊢o  e rator `(-> ,i ,t))
      (⊢*o e rands i))))

(define-relation (typeo term type)
  (⊢o '() term type))

(define-relation (⊢do env term type)
  (conde
   [(numbero term) (== type 'int)]
   [(fresh (ns bd)
      (== term `(λ ,ns ,bd))
      (listo ns)
      (== ))]))

(define-relation (typedo term type)
  (⊢do '() term type))
