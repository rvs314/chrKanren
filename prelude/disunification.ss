#!r6rs

(library (chrKanren prelude disunification)
  (export =/=)
  (import (rnrs)
          (only (srfi :1 lists) filter-map lset<=)
          (chrKanren syntax)
          (chrKanren utils)
          (chrKanren vars)
          (chrKanren goals)
          (chrKanren unify)
          (chrKanren varmap)
          (srfi :2 and-let*)
          (chrKanren compare)
          (chrKanren rule)
          (chrKanren prelude types))

  ;; TODO: this is a bad hack and should likely be replaced
  (define (reified-var? obj)
    (and (symbol? obj)
         (let ([nm (symbol->string obj)])
           (and (> (string-length nm) 3)
                (string=? "_." (substring nm 0 2))))))

  (define-constraint (=/= _l _r) (error '=/= "Should not reify =/="))
  (define-constraint (=/=* alist)
    (define alist*
      (filter-map
       (lambda (l.r)
         (let-values ([(l r) (car+cdr l.r)])
           (and-let* ([vm (unify l r empty-varmap)]
                      [al (varmap->alist vm)]
                      [rt (lex-sort (list (single-out (map car al))
                                          (single-out (map cdr al))))])
             (and (not (equal? rt '(() ())))
                  rt))))
       alist))
    `(=/= ,@alist*))

  (define (equal+lauqe xs ys)
    (or (equal? xs ys)
        (equal? (cons (cdr xs) (car xs)) ys)))

  ;; (=/= ((A . B) (C . D) (E . F)))
  ;; Means either, A ≠ B or C ≠ D or E ≠ F

  (define-rules
    (forall (x y)
      (=/= x y)
      <=>
      (=/=* (list (cons x y))))
    (forall ()
      (=/=* (list))
      <=>
      fail)
    (forall (l ls)
      (=/=* (cons (cons l l) ls))
      <=>
      (=/=* ls))
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (ground atom? l)
      (ground (negate equal?) l r)
      <=>
      succeed)
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (ground atom? r)
      (ground (negate equal?) l r)
      <=>
      succeed)
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (numbero l)
      (symbolo r)
      <=>
      (numbero l)
      (symbolo r))
    (forall (l r rs)
      (=/=* (cons (cons l r) rs))
      (numbero l)
      (symbolo r)
      <=>
      (numbero l)
      (symbolo r))
    (forall (ll lr rl rr rs)
      (=/=* (cons (cons (cons ll lr) (cons rl rr)) rs))
      <=>
      (=/=* (cons* (cons ll rl) (cons lr rr) rs)))))
