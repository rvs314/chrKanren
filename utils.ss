#!r6rs

(library (chrKanren utils)
  (export define-syntax-rule
          TODO
          eta thunk
          *puts-output-port* puts
          car+cdr find-and-remove ref-and-remove
          compose const conjoin disjoin on
          sort merge make-tree=?)
  (import (rnrs)
          (srfi :39 parameters)
          (only (srfi :1 lists) split-at take))

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

  (define (on combine . projs)
    (lambda xs
      (apply combine (map (apply compose projs) xs))))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(define-syntax-rule (name args ...)
         body ...)
       (define-syntax name
         (syntax-rules ()
           [(name args ...)
            (begin body ...)]))]))

  (define-syntax-rule (eta proc)
    (lambda args (apply proc args)))

  (define-syntax-rule (thunk body ...)
    (lambda _ body ...))

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

  (define (ref-and-remove idx lst)
    (cond
      [(null? lst) (values #f        #f)]
      [(zero? idx) (car+cdr lst)]
      [else        (let-values ([(f v) (ref-and-remove (- idx 1) (cdr lst))])
                     (values f (and v (cons (car lst) v))))]))

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
             (merge < (sort < h) (sort < t))))]))

  (define (make-tree=? elem=?)
    (lambda (list1 . more-lists)
      (define lists (cons list1 more-lists))
      (cond
        [(exists null? lists) (for-all null? lists)]
        [(for-all pair? lists)
         (and (apply (make-tree=? elem=?) (map car lists))
              (apply (make-tree=? elem=?) (map cdr lists)))]
        [else (apply elem=? lists)]))))
