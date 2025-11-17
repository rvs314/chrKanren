#!r6rs

(library (chrKanren utils)
  (export define-syntax-rule
          TODO
          atom?
          eta
          *puts-output-port* puts
          car+cdr find-and-remove ref-and-remove
          compose const conjoin disjoin negate on
          and-proc or-proc
          sort merge make-tree=? make-tree-compare
          repeatedly
          hashtable-ref-or-compute!
          make-equal-hashtable
          hashtable->alist
          define/memoized using
          add1 sub1
          fixpoint
          begin0
          treeof pairof listof
          tuple->pair pair->tuple
          symbol
          prioritize-comparators order->comparator comparator->order
          group-by
          single-out)
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

  (define (negate proc)
    (lambda xs
      (not (apply proc xs))))

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

  (define (make-tree-compare elem-compare)
    (define (compare l1 l2)
      (cond
        [(and (null? l1) (null? l2)) '=]
        [(and (null? l1) (pair? l2)) '<]
        [(and (pair? l1) (null? l2)) '>]
        [(and (pair? l1) (pair? l2))
         (let* ([o1 (compare (car l1) (car l2))])
           (if (eq? o1 '=)
               (compare (cdr l1) (cdr l2))
               o1))]
        [(or (null? l1) (pair? l1)) '>]
        [(or (null? l2) (pair? l2)) '<]
        [else (elem-compare l1 l2)]))
    compare)

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
         (using (make-equal-hashtable))
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

  (define-syntax using (syntax-rules ()))

  (define (add1 x) (+ x 1))
  (define (sub1 x) (- x 1))

  (define-syntax-rule (begin0 v0 v ...)
    (let ([r v0])
      v ...
      r))

  (define (pairof car? cdr?)
    (conjoin pair?
             (compose car? car)
             (compose cdr? cdr)))

  (define (listof elem?)
    (disjoin null?
             (pairof elem? (eta (listof elem?)))))

  (define (treeof elem?)
    (disjoin elem?
             (pairof (eta (treeof elem?)) (eta (treeof elem?)))))

  (define atom?
    (disjoin null? number? string? char?
             boolean? symbol? bytevector?))

  (define (fixpoint step finished? start0 . start)
    (let-values ([next (apply step start0 start)])
      (if (apply finished? start0 (append start next))
          (apply values next)
          (apply fixpoint step finished? next))))

  (define (tuple->pair obj)
    (cons (car obj) (cadr obj)))

  (define (pair->tuple obj)
    (list (car obj) (cdr obj)))

  (define show
    (case-lambda
      [(obj mthd)
       (call-with-string-output-port
        (lambda (sop)
          (mthd obj sop)))]
      [(obj) (show obj display)]))

  (define (symbol . objs)
    (string->symbol (apply string-append (map show objs))))

  (define (order->comparator predicate)
    (lambda (l r)
      (cond
        [(not (predicate l r)) '>]
        [(not (predicate r l)) '<]
        [else                  '=])))

  (define (comparator->order comp)
    (lambda (l r)
      (case (comp l r)
        [(< =) #t]
        [(>)   #f]
        [else (error 'comparator->order "Not an order" comp)])))

  (define (prioritize-comparators . os)
    (if (null? os)
        (const '=)
        (lambda (x y)
          (let* ([o  (car os)]
                 [r  (o x y)]
                 [os (cdr os)])
            (if (eq? r '=)
                ((apply prioritize-comparators os) x y)
                r)))))

  (define (prioritize o1 . os)
    (lambda (x y)
      (let ([-> (and (o1 x y) #t)]
            [<- (and (o1 y x) #t)])
        (cond
          [(eq? -> <-)      ((apply prioritize os) x y)]
          [(and -> (not <-)) #t]
          [(and (not ->) <-) #f]
          [else (error 'prioritize "Impossible")]))))

  (define (hashtable->alist ht)
    (unless (hashtable? ht)
      (error 'hashtable->alist "hashtable->alist argument must be a hashtable" ht))
    (let-values ([(v1 v2) (hashtable-entries ht)])
      (map cons (vector->list v1) (vector->list v2))))

  (define make-equal-hashtable
    (case-lambda
      [(size) (make-hashtable equal-hash equal? size)]
      [() (make-hashtable equal-hash equal?)]))

  (define (group-by proj objs)
    (hashtable->alist
     (fold-left
      (lambda (acc obj)
        (let-values ([(key val)
                      (call-with-values
                       (lambda () (proj obj))
                       (case-lambda
                         [(f)   (values f obj)]
                         [(f g) (values f g)]))])
          (hashtable-update! acc key (lambda (x) (cons val x)) '())
          acc))
      (make-equal-hashtable)
      objs)))

  (define (single-out obj)
    (if (and (list? obj) (= 1 (length obj)))
        (car obj)
        obj)))
