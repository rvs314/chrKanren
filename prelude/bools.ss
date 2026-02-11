#!r6rs

(library (chrKanren prelude bools)
  (export)
  (import (rnrs)
          (chrKanren vars)
          (chrKanren base)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren rule)
          (chrKanren prelude lists))

  (define-constraint (ando x y x∧y))
  (define-constraint (oro x y x∨y))

  (define-rules
    (forall (x y) (ando  x  y #t) <=> (== x #t) (== y #t))
    (forall (x y) (ando  x #f  y) <=> (== y #f))
    (forall (x y) (ando #f  x  y) <=> (== y #f))
    (forall (x y) (ando #t  x  y) <=> (== x y))
    (forall (x y) (ando  x #t  y) <=> (== x y))
    (forall (x y) (ando  x  x  y) <=> (== x y)))

  (define-rules
    (forall (x y) (oro #t x  y) <=> (== y #t))
    (forall (x y) (oro x #t  y) <=> (== y #t))
    (forall (x y) (oro #f x  y) <=> (== x y))
    (forall (x y) (oro x #f  y) <=> (== x y))
    (forall (x y) (oro x  y #f) <=> (== x #f) (== y #f))
    (forall (x y) (oro x  x  y) <=> (== x y)))

  (define-constraint (xoro x ¬x))

  (define-rules
    (forall (x)     (xoro x #t)           <=> (== x #f))
    (forall (x)     (xoro x #f)           <=> (== x #t))
    (forall (x)     (xoro #t x)           <=> (== x #f))
    (forall (x)     (xoro #f x)           <=> (== x #t))
    (forall (x)     (xoro x x)            <=> fail)
    (forall (x y z) (xoro x y) (xoro y z) <=> (xoro x y) (== x z)))

  )
