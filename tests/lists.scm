#!r6rs

(import (rnrs)
        (chrKanren base)
        (chrKanren prelude types)
        (chrKanren prelude lists)
        (chrKanren check)
        (chrKanren test))

(define-relation (snoco xs x rs)
  (conso x xs rs))

(define-test test-foldo
  (check (equal?
          (run 5 (p q r s)
            (fold-lefto p q r s))
          '(((_.0 _.1 () _.1))
            ((_.0 _.1 (_.2) _.3)
             (applyo _.0 (_.1 _.2 _.3))
             (rel _.0))
            ((_.0 _.1 (_.2 _.3) _.4)
             (applyo _.0 (_.1 _.2 _.5))
             (applyo _.0 (_.5 _.3 _.4))
             (rel _.0))
            ((_.0 _.1 (_.2 _.3 _.4) _.5)
             (applyo _.0 (_.1 _.2 _.6))
             (applyo _.0 (_.6 _.3 _.7))
             (applyo _.0 (_.7 _.4 _.5))
             (rel _.0))
            ((_.0 _.1 (_.2 _.3 _.4 _.5) _.6)
             (applyo _.0 (_.1 _.2 _.7))
             (applyo _.0 (_.7 _.3 _.8))
             (applyo _.0 (_.8 _.4 _.9))
             (applyo _.0 (_.9 _.5 _.6))
             (rel _.0)))))
  (check (equal?
          (run* (p)
            (fold-lefto snoco '() '(1 2 3 4 5) p))
          '((((5 4 3 2 1)))))))

(define-test test-listo
  (check (equal?
          (run* (p) (listo p))
          '(((_.0) (lst _.0)))))
  (check (equal?
          (run* (p) (listo p) (== 2 p))
          '()))
  (check (equal?
          (run* (p) (listo p) (fresh (z) (== (cons 2 z) p)))
          '((((2 . _.0)) (lst _.0))))))
