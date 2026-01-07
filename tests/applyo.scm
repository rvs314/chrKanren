#!r6rs

(import (rnrs)
        (chrKanren base)
        (chrKanren utils)
        (chrKanren prelude types)
        (chrKanren test)
        (chrKanren generator)
        (chrKanren check)
        (chrKanren compare))

(define-relation (farmo animal)
  (conde
   [(== animal 'cow)]
   [(== animal 'pig)]
   [(== animal 'chicken)]
   [(== animal 'goat)]
   [(== animal 'rabbit)]))

(define-relation (peto animal)
  (conde
   [(== animal 'cat)]
   [(== animal 'dog)]
   [(== animal 'turtle)]
   [(== animal 'gerbil)]
   [(== animal 'rabbit)]))

(define-relation (next-smallesto a1 a2)
  (conde
   [(== a1 'gerbil)  (== a2 'turtle)]
   [(== a1 'turtle)  (== a2 'rabbit)]
   [(== a1 'rabbit)  (== a2 'chicken)]
   [(== a1 'chicken) (== a2 'cat)]
   [(== a1 'cat)     (== a2 'dog)]
   [(== a1 'dog)     (== a2 'goat)]
   [(== a1 'goat)    (== a2 'pig)]
   [(== a1 'pig)     (== a2 'cow)]))

(define-relation (size<=o a1 a2)
  (conde
   [(== a1 a2)]
   [(fresh (x)
      (next-smallesto a1 x)
      (size<=o x a2))]))

(define-test relationo-type
  (check (equal?
          (run* (p)
            (relationo p))
          '(((_.0) (rel _.0)))))
  (check (equal?
          (run* (p)
            (relationo p)
            (== p farmo))
          `(((,farmo)))))
  (check (equal?
          (run* (p)
            (fresh (q)
              (relationo q)))
          '(((_.0)))))
  (check (equal?
          (run* ()
            (fresh (q)
              (symbolo q)
              (relationo q)))
          '()))
  (check (equal?
          (run* (p)
            (== p farmo)
            (symbolo p))
          '()))
  (check (equal?
          (run* (p)
            (== p farmo)
            (relationo p))
          `(((,farmo)))))
  (check (equal?
          (run* (p q)
            (relationo p)
            (== p q))
          '(((_.0 _.0) (rel _.0)))))
  (check (equal?
          (run* (p)
            (relationo p)
            (symbolo p))
          '()))
  (check (equal?
          (run* (p)
            (=/= p farmo))
          `(((_.0) (=/= (_.0 ,farmo))))))
  (check (equal?
          (run* ()
            (=/= farmo farmo))
          '()))
  (check (equal?
          (run* ()
            (=/= peto farmo))
          '((()))))
  (check (equal?
          (run* ()
            (== peto peto))
          '((()))))
  (check (equal?
          (run* ()
            (== peto farmo))
          '()))
  (check (equal?
          (run* (p)
            (=/= peto p)
            (symbolo p))
          '(((_.0) (sym _.0))))))

(define-test applyo-test
  (check (equal?
          (run* (p q)
            (applyo p q))
          '(((_.0 _.1) (applyo _.0 _.1) (rel _.0)))))
  (check (equal?
          (run* (p q r s t)
            (applyo p q r s t))
          '(((_.0 _.1 _.2 _.3 _.4) (applyo _.0 (_.1 _.2 _.3 . _.4)) (rel _.0)))))
  (check (equal?
          (run* (p)
            (callo farmo p))
          '(((cow)) ((pig)) ((chicken)) ((goat)) ((rabbit)))))
  (check (equal?
          (run* (p)
            (fresh (q)
              (callo q p)
              (== q farmo)))
          '(((cow)) ((pig)) ((chicken)) ((goat)) ((rabbit)))))
  (check (equal?
          (run* (p)
            (callo farmo p)
            (callo peto p))
          '(((rabbit)))))
  (check (equal?
          (run* (p)
            (fresh (q)
              (callo q p)
              (conde
               [(== q farmo)]
               [(== q peto)])))
          '(((cow))
            ((cat))
            ((pig))
            ((dog))
            ((chicken))
            ((turtle))
            ((goat))
            ((gerbil))
            ((rabbit))
            ((rabbit)))))
  (check (equal? (run* (p q)
                   (callo next-smallesto p q))
                 '(((gerbil turtle))
                   ((turtle rabbit))
                   ((rabbit chicken))
                   ((chicken cat))
                   ((cat dog))
                   ((dog goat))
                   ((goat pig))
                   ((pig cow)))))
  (check (equal? (run* (p)
                   (callo next-smallesto 'cat p))
                 '(((dog)))))
  (check (equal? (run* (p)
                   (callo size<=o 'cat p))
                 '(((cat)) ((dog)) ((goat)) ((pig)) ((cow)))))
  (check (equal? (run* (p)
                   (applyo size<=o 'cat (list p)))
                 '(((cat)) ((dog)) ((goat)) ((pig)) ((cow)))))
  (check (equal? (run* (p)
                   (callo size<=o 'cat p))
                 '(((cat)) ((dog)) ((goat)) ((pig)) ((cow)))))
  ;; FIXME: This is a dumb restriction: we should be able to infer the arity
  ;; of the relation and force the arglists to unify.
  (check (equal? (run* (p)
                   (applyo size<=o 'cat p))
                 `(((_.0) (applyo ,size<=o (cat . _.0)))))))
