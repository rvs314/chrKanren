#!r6rs

(library (chrKanren prelude applyo)
  (export callo applyo relationo)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren prelude types)
          (chrKanren base))

  (define-constraint (relationo obj)
    (error 'relationo "Should not reify"))

  (define-constraint (%applyo proc args)
    `(applyo ,proc ,args))

  (define (applyo rel . arg-then-args)
    (let-values ([(final initial) (ref-and-remove (- (length arg-then-args) 1)
                                                  arg-then-args)])
      (conj (relationo rel)
            (%applyo rel (append initial final)))))

  (define (callo rel . args)
    (applyo rel args))

  (define-rules
    (forall (x) (relationo x) <=> (typeo x 'rel procedure?))
    (forall (x y)
      (%applyo x y)
      (ground list? y)
      (ground procedure? x)
      <=>
      (apply x y))
    (forall (x y)
      (== x y)
      (ground procedure? x)
      (ground procedure? y)
      (ground (negate eq?) x y)
      <=>
      fail)
    (forall (x y)
      (=/= x y)
      (ground procedure? x)
      (ground procedure? y)
      (ground eq? x y)
      <=>
      fail)))
