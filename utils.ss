#!r6rs

(library (chrKanren utils)
  (export define-syntax-rule
          TODO
          *puts-output-port* puts
          car+cdr find-and-remove
          compose const conjoin disjoin
          sort merge)
  (import (rnrs)
          (srfi :39 parameters)
          (only (srfi :1 lists) split-at))

  (define (conjoin . fns)
    (lambda xs
      (for-all (lambda (f) (apply f xs)) fns)))

  (define (disjoin . fns)
    (lambda xs
      (exists (lambda (f) (apply f xs)) fns)))

  (define (const . xs)
    (lambda _
      (apply values xs)))

  (define (compose . fs)
    (define (compose₂ f1 f2)
      (lambda xs (call-with-values (lambda () (apply f1 xs)) f2)))
    (fold-left compose₂ values (reverse fs)))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(define-syntax-rule (name args ...)
         body ...)
       (define-syntax name
         (syntax-rules ()
           [(name args ...)
            (begin body ...)]))]))

  (define-syntax TODO
    (identifier-syntax
     (error 'TODO "Code incomplete")))

  (define *puts-output-port*
    (make-parameter (current-output-port)))

  (define puts
    (case-lambda
      [()
       (newline (*puts-output-port*))]
      [(obj)
       (write obj (*puts-output-port*))
       (newline (*puts-output-port*))
       obj]
      [(obj1 obj2 . more)
       (write obj1 (*puts-output-port*))
       (display " " (*puts-output-port*))
       (apply puts obj2 more)]))

  (define (car+cdr pr)
    (values (car pr) (cdr pr)))

  (define (find-and-remove pred lst)
    (cond
      [(null? lst)      (values #f        #f)]
      [(pred (car lst)) (car+cdr lst)]
      [else             (let-values ([(f v) (find-and-remove pred (cdr lst))])
                          (values f (and v (cons (car lst) v))))]))

  ;; These aren't in the R6RS spec, unfortunately.
  ;; They are implemented by SRFIs 132 and 32, but
  ;; Chez only implements the former and Racket only the later.

  (define merge
    (case-lambda
      [(l1 l2) (merge < l1 l2)]
      [(< l1 l2)
       (cond
         [(null? l1) l2]
         [(null? l2) l1]
         [(< (car l1) (car l2)) (cons (car l1) (merge < (cdr l1) l2))]
         [else                  (cons (car l2) (merge < (cdr l2) l1))])]))

  (define sort
    (case-lambda
      [(lst) (sort < lst)]
      [(< lst)
       (if (or (null? lst) (null? (cdr lst)))
           lst
           (let*-values ([(ln)  (length lst)]
                         [(h t) (split-at lst (floor (/ ln 2)))])
             (merge < (sort < h) (sort < t))))])))
