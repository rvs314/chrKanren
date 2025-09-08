#!r6rs

(import (rnrs) (chrKanren vars) (srfi :64) (srfi :39))

(test-begin "vars")

(define v1 (make-var 'v1))
(define v2 (make-var 'v2))

(test-eq "vars eq? to themselves" v1 v1)
(test-assert "vars not eq? to others" (not (eq? v1 v2)))
(test-assert "vars not equal? to others" (not (equal? v1 v2)))
(test-assert "vars not equal? to others" (not (equal? v1 v2)))
(test-eq "var-name in a var obj" (var-name v1) 'v1)

(fresh (p q)
  (test-eq "var eq? to itself" p p)
  (test-assert "var not equal? to others" (not (equal? p q)))
  (test-eq "var-name in a fresh" (var-name p) 'p))

(*var-counter* 0)
(test-eqv "var coutner at zero" (var-idx (make-var 'the-var-counter)) 0)
(test-eqv "var counter at 1" (*var-counter*) 1)

(parameterize ([*var-counter* 0])
  (test-eqv "var counter in param/1" (var-idx (make-var 'foo)) 0)
  (test-eqv "var counter in param/2" (var-idx (make-var 'bar)) 1)
  (test-eqv "var counter in param/3" (var-idx (make-var 'baz)) 2))

(test-eqv "var counter at 1 after param" (*var-counter*) 1)

(test-end)
