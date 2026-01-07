#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren utils)
        (chrKanren base)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren reifier)
        (chrKanren streams)
        (chrKanren interp)
        (chrKanren unify)
        (only (srfi :1 lists) lset=))

(define-relation (animalo obj)
  (conde
   [(== obj 'cow)]
   [(== 'goat obj)]
   [succeed (== 'pig obj)]
   [(== obj 'robot) fail]))

(define the-animals '(pig goat cow))

(define-test test-animalo
  (check (lset= equal?
                (map (compose list list) the-animals)
                (run* (p) (animalo p))))
  (for-each
   (lambda (animal)
     (check (pair? (run* () (animalo animal)))))
   the-animals)
  (check (null? (run* () (animalo 'robot)))))

(define-relation (chaino xs ys)
  (fresh (a b c d)
    (== xs a)
    (== a b)
    (== b c)
    (== d c)
    (== d ys)))

(define-test test-chaino
  (check (equal? (run* (p q) (chaino p q))
                 '(((_.0 _.0))))))

(define-relation (appendo xs ys zs)
  (conde
   [(== xs '()) (== ys zs)]
   [(fresh (x xss zss)
      (== xs (cons x xss))
      (== zs (cons x zss))
      (appendo xss ys zss))]))

(define-test test-appendo
  (check
   (equal? (run-finite 3 () (appendo '(1 2 3) '(4 5 6) '(1 2 3 4 5 6)))
           '((()))))

  (check
   (equal? (run*-finite (p) (appendo p '(4 5 6) '(1 2 3 4 5 6)))
           '((((1 2 3))))))

  (check
   (equal? (run*-finite (p q) (appendo p q '(1 2 3 4 5 6))) 
           '(((() (1 2 3 4 5 6)))
             (((1) (2 3 4 5 6)))
             (((1 2) (3 4 5 6)))
             (((1 2 3) (4 5 6)))
             (((1 2 3 4) (5 6)))
             (((1 2 3 4 5) (6)))
             (((1 2 3 4 5 6) ())))))

  (check
   (equal? (run-finite 3 (p q r) (appendo p q r))
           '(((() _.0 _.0))
             (((_.0) _.1 (_.0 . _.1)))
             (((_.0 _.1) _.2 (_.0 _.1 . _.2)))))))

