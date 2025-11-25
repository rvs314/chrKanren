#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren test)
        (chrKanren generator)
        (chrKanren check)
        (chrKanren compare))

(define-test manual-test-atom<=?
  (check (lex<=? 2 3))
  (check (lex<=? 2 +inf.0))
  (check (lex<=? -3 +inf.0))
  (check (lex<=? "ant" "bat"))
  (check (lex<=? 0 "bat"))
  (check (lex<=? 0 'zero)))


(define-test (lex<=?-over-lists (obj1 value-generator)
                                (obj2 value-generator))
  (if (lex<=? obj1 obj2)
      (lex<=? (list obj1) (list obj2))
      (lex<=? (list obj2) (list obj1))))

(define-test maual-test-lex<=?
  (check (not (lex<=? '((_.0 (3 4 . 5))) '((_.0 3)))))
  (check (lex<=? '((_.0 3)) '((_.0 (3 4 . 5)))))
  (check
   (equal? (lex-sort '(((_.0 (3 4 . 5))) ((_.0 3))))
           '(((_.0 3)) ((_.0 (3 4 . 5)))))))
