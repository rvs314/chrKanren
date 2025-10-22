#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren base)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren streams)
        (chrKanren subst)
        (chrKanren interp)
        (chrKanren unify)
        (only (srfi :1 lists) lset=))

(define-relation (animalo obj)
  (conde
   [(== obj 'cow)]
   [(== 'goat obj)]
   [succeed (== 'pig obj)]
   [(== obj 'robot) fail]))

(define-test test-animalo
  (define the-animals '(pig goat cow))
  (check (lset= equal? the-animals (map car (run* (p) (animalo p)))))
  (for-each
   (lambda (animal)
     (check (equal? (run* () (animalo animal)) '(()))
            "concrete animal"
            animal))
   the-animals))

(define-relation (chaino xs ys)
  (fresh (a b c d)
    (== xs a)
    (== a b)
    (== b c)
    (== d c)
    (== d ys)))

(define-test test-chaino
  (define res (run* (p q) (chaino p q)))
  (check (= (length res) 1))
  (check (eq? (caar res) (cadar res))))

(define-relation (appendo xs ys zs)
  (conde
   [(== xs '()) (== ys zs)]
   [(fresh (x xss zss)
      (== xs (cons x xss))
      (== zs (cons x zss))
      (appendo xss ys zss))]))

(define-test test-appendo
  (define res (run-finite 10 (p q r) (appendo p q r)))
  res)

