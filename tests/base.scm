#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren base)
        (only (srfi :1 lists) lset=))

(define-relation (animalo obj)
  (conde
   [(== obj 'cow)]
   [(== 'goat obj)]
   [(== 'pig obj)]))

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
  (fresh (a)
    (== xs a)
    (== a ys)))

(define-test test-chaino
  (define res (run* (p q) (chaino p q)))
  (check (= (length res) 1))
  (check (eq? (caar res) (cadar res))))

#;
(define-relation (appendo xs ys zs)
  (conde
   [(== xs '()) (== ys zs)]
   [(fresh (x xss zss)
      (== xs (cons x xss))
      (== zs (cons x zss))
      (appendo xss ys zss))]))

