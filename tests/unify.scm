#!r6rs

(import (rnrs)
        (chrKanren vars) (chrKanren unify)
        (chrKanren subst) (chrKanren goals)
        (chrKanren streams) (chrKanren test)
        (chrKanren interp)
        (srfi :64 testing))

(test-begin "unify")

(define-random-test (smoke-tests [r (random-state)])
  #;(test-eq "(== 1 2) fails" empty-stream (unify 1 2 r))
  (test-equal "(== 1 1) succeeds" (take-finite (unify 1 1 r))))

(test-end "unify")
