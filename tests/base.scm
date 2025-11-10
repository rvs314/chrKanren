#!r6rs

(import (rnrs)
        (chrKanren test)
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

(define-test test-animalo
  (define the-animals '(pig goat cow))
  (check (lset= equal? the-animals
                (map car (run* (p) (animalo p)))))
  (for-each
   (lambda (animal)
     (check (pair? (run* () (animalo animal)))
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
  (check
   (equal? (run-finite 3 () (appendo '(1 2 3) '(4 5 6) '(1 2 3 4 5 6)))
           '(())))

  (check
   (equal? (run*-finite (p) (appendo p '(4 5 6) '(1 2 3 4 5 6)))
           '(((1 2 3)))))

  (check
   (equal? (run*-finite (p q) (appendo p q '(1 2 3 4 5 6))) 
           '((() (1 2 3 4 5 6))
             ((1) (2 3 4 5 6))
             ((1 2) (3 4 5 6))
             ((1 2 3) (4 5 6))
             ((1 2 3 4) (5 6))
             ((1 2 3 4 5) (6))
             ((1 2 3 4 5 6) ()))))

  (check
   (equal? (run-finite 3 (p q r) (appendo p q r))
           '((() _.0 _.0)
             ((_.0) _.1 (_.0 . _.1))
             ((_.0 _.1) _.2 (_.0 _.1 . _.2))))))

