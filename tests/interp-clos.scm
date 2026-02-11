#!r6rs

(import (rnrs)
        (chrKanren base)
        (chrKanren prelude lists)
        (chrKanren prelude types))

(define-relation (evalo term env val)
  (conde
   [(fresh (nm bd)
      (== term `(λ ,nm ,bd))
      (== val (lambda (input output)
                (evalo bd (cons (cons nm input) env) output))))]
   [(fresh (rator rator^ rand rand^)
      (== term `(,rator ,rand))
      (evalo rator env rator^)
      (evalo rand env rand^)
      (callo rator^ rand^ val))]
   [(symbolo term) (lookupo term env val)]
   [(== term `(quote ,val))]))
