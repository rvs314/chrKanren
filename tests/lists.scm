#!r6rs

(import (rnrs)
        (chrKanren base)
        (chrKanren prelude applyo)
        (chrKanren prelude lists)
        (chrKanren check)
        (chrKanren test))

(define-relation (snoco xs x rs)
  (conso x xs rs))

(define-test test-foldo
  (check (equal?
          (run* (p)
            (fold-lefto snoco '() '(1 2 3 4 5) p))
          '((((5 4 3 2 1))))))
  (check (equal?
          (run* (p)
            (fold-lefto snoco '() p '(1 2 3 4 5)))
          '((((5 4 3 2 1)))))))
