#!r6rs

(library (chrKanren compare)
  (export lex-sort)
  (import (rnrs)
          (only (srfi :1 lists) list-index)
          (chrKanren utils)
          (chrKanren vars))

  (define (boolean<=? left right)
    (or right (not left)))

  (define atomic-type-table
    `((,var?     . ,var<=?)
      (,boolean? . ,boolean<=?)
      (,number?  . ,<=)
      (,string?  . ,string<=?)
      (,symbol?  . ,(on string<=? symbol->string))))

  (define (atom-compare left right)
    (define li (list-index (lambda (entry) ((car entry) left))  atomic-type-table))
    (define ri (list-index (lambda (entry) ((car entry) right)) atomic-type-table))
    (cond
      [(or (not li) (not ri))
       (error 'atom<=? "atom<=? only compares atoms" left right)]
      [(< li ri) '<]
      [(> li ri) '>]
      [(= li ri) ((order->comparator (cdr (assp (lambda (f) (f left)) atomic-type-table)))
                  left
                  right)]))

  (define lex-compare (make-tree-compare atom-compare))
  (define (lex-sort lst) (sort (comparator->order lex-compare) lst)))
