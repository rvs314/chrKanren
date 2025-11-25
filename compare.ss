#!r6rs

(library (chrKanren compare)
  (export boolean<=? compare eqv<=? prioritize
          restrict make-pair<=? atom<=? make-lex<=? lex<=? lex-sort)
  (import (rnrs)
          (only (srfi :1 lists) reduce-right)
          (chrKanren utils)
          (chrKanren check)
          (chrKanren vars))

  #|
  A comparator is a procedure of two arguments which returns
  a boolean (#t or #f). This procedure represents a decision procedure
  for the less-than-or-equal-to relation of some partial order.

  If (not (or (<= x y) (<= y x))), then x and y are said to be incomparable.
  If (xor (<= x y) (<= y x)), then x and y are said to be inequal, but comparable.
  If (and (<= x y) (<= y x)), then x and y are said to be equal.

  Note: `equal?` is a comparator for the discrete partial order
  over the universe set. Also, `(<= x x)` is #f iff `x` is not in
  the domain of `<=`.
  |#

  (define (restrict dom? <=?)
    (lambda (x y)
      (and (dom? x)
           (dom? y)
           (<=? x y))))

  (define (eqv-to obj)
    (lambda (os)
      (eqv? obj os)))

  (define (eqv<=? dom?)
    (restrict dom? eqv?))

  (define (boolean<=? left right)
    (or right (not left)))

  (define (compare <=? left right)
    (define <= (<=? left right))
    (define >= (<=? right left))
    (cond
      [(and <=       >=)       '=]
      [(and (not <=) >=)       '>]
      [(and <=       (not >=)) '<]
      [(and (not <=) (not >=)) #f]
      [else (error 'compare "Impossible")]))

  (define (prioritize . cs)
    (define (prioritize₂ <=1 <=2)
      (lambda (x y)
        (cond
          [(<=1 x y) #t]
          [(<=1 y x) #f]
          [(and (<=1 x x) (not (<=1 y y))) #t]
          [(and (<=1 y y) (not (<=1 x x))) #f]
          [else (<=2 x y)])))
    (reduce-right prioritize₂ (const #f) cs))

  (define atom<=?
    (prioritize
     (restrict var? var<=?)
     (restrict number? <=)
     (restrict string? string<=?)
     (restrict symbol? (on string<=? symbol->string))
     (restrict boolean? boolean<=?)
     (restrict null? eq?)))

  (define (make-pair<=? car<=? cdr<=?)
    (restrict pair?
              (lambda (x y)
                (case (compare car<=? (car x) (car y))
                  [(<)  #t]
                  [(>)  #f]
                  [(=) (cdr<=? (cdr x) (cdr y))]
                  [(#f) (let ([dx (car<=? (car x) (car x))]
                              [dy (car<=? (car y) (car y))])
                          (cond
                            [(and dx (not dy)) #t]
                            [(and dy (not dx)) #f]
                            [(and dx dy)       #f]
                            [else              (cdr<=? (cdr x) (cdr y))]))]
                  [else (check #f
                               "What?"
                               (compare car<=? (car x) (car y)))]))))

  (define (make-lex<=? elem<=?)
    (define rec (eta (make-lex<=? elem<=?)))
    (prioritize
     elem<=?
     (restrict null? eq?)
     (make-pair<=? rec rec)))

  (define lex<=? (make-lex<=? atom<=?))
  (define (lex-sort lst) (sort lex<=? lst)))
