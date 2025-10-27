#!r6rs

(library (chrKanren utils)
  (export define-syntax-rule
          TODO
          eta
          *puts-output-port* puts
          car+cdr find-and-remove ref-and-remove
          compose const conjoin disjoin on
          and-proc or-proc
          sort merge make-tree=?
          repeatedly
          hashtable-ref-or-compute!
          define/memoized using
          add1 sub1
          begin0)
  (import (rnrs)
          (srfi :39 parameters)
          (only (srfi :1 lists) split-at take reduce))


  (define (and-proc . objs)
    (reduce (lambda (x y) (and x y)) #t objs))

  (define (or-proc . objs)
    (reduce (lambda (x y) (or x y)) #f objs))

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
        [else (apply elem=? lists)])))

  (define (repeatedly k thnk)
    (let loop ([i 0])
      (when (< i k)
        (thnk i)
        (loop (+ i 1)))))

  (define tag (list 'tag))

  (define (hashtable-ref-or-compute! hashtable key on-failure)
    (define res #f)
    (hashtable-update! hashtable
                       key
                       (lambda (obj)
                         (set! res
                               (if (eq? obj tag)
                                   (on-failure)
                                   obj))
                         res)
                       tag)
    res)

  (define-syntax define/memoized
    (syntax-rules (using)
      [(define/memoized (name)
         body body* ...)
       (define name
         (let ([set? #f]
               [val  #f])
           (lambda ()
             (if set?
                 val
                 (let ([result (begin body body* ...)])
                   (set! set? #t)
                   (set! val result)
                   result)))))]
      [(define/memoized (name arg)
         (using obj)
         body body* ...)
       (define name
         (let ([the-hashtable obj])
           (lambda (arg)
             (hashtable-ref-or-compute!
              the-hashtable
              arg
              (lambda () body body* ...)))))]
      [(define/memoized (name arg)
         body body* ...)
       (define/memoized (name arg)
         (using (make-eqv-hashtable))
         body body* ...)]
      [(define/memoized (name . args)
         (using obj)
         body body* ...)
       (define name
         (let ([the-hashtable obj])
           (lambda args
             (hashtable-ref-or-compute!
              the-hashtable
              args
              (lambda () body body* ...)))))]))

  (define-syntax using
    (identifier-syntax
     (error 'using "Invalid if used outside of a `define/memozied` block")))

  (define (add1 x) (+ x 1))
  (define (sub1 x) (- x 1))

  (define-syntax-rule (begin0 v0 v ...)
    (let ([r v0])
      v ...
      r)))
