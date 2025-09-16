#!r6rs

(import (rnrs)
        (chrKanren utils)
        (only (srfi :1 lists) make-list)
        (srfi :64 testing)
        (srfi :39 parameters)
        (srfi :27 random-bits)
        (srfi :26 cut))

(test-begin "utils")

(define-syntax-rule (foo bar baz)
  (list bar bar))

(test-equal "define-syntax-rule"
  (call-with-string-output-port
   (lambda (op)
     (foo (display "1" op) (display "2" op))))
  "11")

(test-error ("TODO" error?) TODO)

(test-equal "puts"
  (call-with-string-output-port
   (lambda (op)
     (parameterize ([*puts-output-port* op])
       (puts 1 "two" 'three))))
  "1 \"two\" three\n")

(let*-values ([(k)   (cons 'car 'cdr)]
              [(a d) (car+cdr k)])
  (test-group "car+cdr"
    (test-eq a 'car)
    (test-eq d 'cdr)))

(test-group "find-and-remove"
  (let-values ([(k e) (find-and-remove even? '(1 2 3 4))])
    (test-eqv k 2)
    (test-equal e '(1 3 4)))
  (let-values ([(k e) (find-and-remove even? '(1 3 5))])
    (test-eq #f k)
    (test-eq #f e)))

(test-group "compose"
  (test-equal 9
    ((compose) 9))
  (test-equal '#((#t))
    ((compose vector list even? (cut + 1 <>)) 3))
  (test-equal 9
    ((compose + car+cdr cons) 4 5)))

(test-group "conjoin"
  (test-equal #t ((conjoin number? even?) 4))
  (test-equal #t
    ((conjoin pair? (compose even? car) (compose odd? cdr)) '(4 . 3)))
  (test-equal #f ((conjoin list? vector?) 9)))

(test-group "disjoin"
  (test-equal #t ((disjoin number? string?) 4))
  (test-equal #t ((disjoin number? string?) "five"))
  (test-equal #t ((disjoin pair? even?) '(4 . 3)))
  (test-equal #f ((disjoin list? vector? char? boolean?) 9))
  (test-equal #f ((disjoin > =) 3 9))
  (test-equal #t ((disjoin < =) 3 9))
  (test-equal #t ((disjoin < =) 3 3)))

(test-group "const"
  (let-values ([xs ((const 1 2))]
               [ys ((const))]
               [zs ((const 1))])
    (test-equal xs '(1 2))
    (test-equal ys '())
    (test-equal zs '(1))))

(define (sorted? lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (<= (car lst) (cadr lst))
           (sorted? (cdr lst)))))

(define (random-int-list)
  (map (lambda (_) (- (random-integer 60) 30)) (make-list (random-integer 30))))

(test-group "sort"
  (do [(i 30 (- i 1))]
      [(< i 0)]
    (test-assert (string-append "sort/" (number->string (- 30 i)))
      (sorted? (sort (random-int-list))))))

(test-end)
