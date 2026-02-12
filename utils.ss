#!r6rs

(library (chrKanren utils)
  (export define-syntax-rule
          TODO
          atom? any?
          eta
          *puts-output-port* puts
          car+cdr find-and-remove ref-and-remove
          compose const conjoin disjoin negate on
          natural?
          and-proc or-proc
          sort merge make-tree=?
          repeatedly
          hashtable-ref-or-compute!
          make-equal-hashtable
          hashtable->alist
          define/memoized using
          add1 sub1
          fixpoint
          begin0
          treeof pairof listof arguments
          tuple->pair pair->tuple
          symbol
          group-by
          singleton? single-out
          find-subtree
          false-map
          vector-set vector-update
          vector-exists vector-fold
          snoc rdc rac rdc+rac
          proccall
          zippers
          copy-object replace-in-object!
          named-lambda)
  (import (rnrs)
          (rnrs mutable-pairs)
          (srfi :39 parameters)
          (srfi :26 cut)
          (only (srfi :1 lists)
                split-at take reduce car+cdr last drop-right))

  (define (and-proc . objs)
    (reduce (lambda (x y) (and x y)) #t objs))

  (define (or-proc . objs)
    (reduce (lambda (x y) (or x y)) #f objs))

  (define (conjoin . fns)
    (lambda xs
      (for-all (cut apply <> xs) fns)))

  (define natural? (conjoin exact? positive? integer?))

  (define (disjoin . fns)
    (lambda xs
      (exists (cut apply <> xs) fns)))

  (define negate (cut compose not <>))

  (define (const . xs)
    (lambda _
      (apply values xs)))

  (define (compose . fs)
    (define (compose₂ f1 f2)
      (lambda xs (call-with-values (cut apply f1 xs) f2)))
    (fold-left compose₂ values (reverse fs)))

  (define (on combine . projs)
    (lambda xs
      (apply combine (map (apply compose projs) xs))))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(define-syntax-rule (name . args)
         body ...)
       (define-syntax name
         (syntax-rules ()
           [(name . args)
            (begin body ...)]))]))

  (define-syntax-rule (eta proc)
    (cut proc <...>))

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
         [else                  (cons (car l2) (merge < l1 (cdr l2)))])]))

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

  (define (find-subtree pred? tree)
    (cond
      [(pred? tree) tree]
      [(vector? tree)
       (vector-exists (lambda (o) (find-subtree pred? o)) tree)]
      [(pair? tree) (or (find-subtree pred? (car tree))
                        (find-subtree pred? (cdr tree)))]
      [else #f]))

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
    (lambda (x)
      (and (list? x) (for-all elem? x))))

  (define (arguments . tests)
    (lambda args
      (and (equal? (length args) (length tests))
           (for-all proccall tests args))))

  (define (treeof elem?)
    (define (matches? obj)
      (or (elem? obj)
          (and (pair? obj)
               (matches? (car obj))
               (matches? (cdr obj)))))
    matches?)

  (define atom?
    (disjoin null? number? string? char?
             boolean? symbol? bytevector?
             procedure?))

  (define (any? . _) #t)

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
        (cut mthd obj <>))]
      [(obj) (show obj display)]))

  (define (symbol . objs)
    (string->symbol (apply string-append (map show objs))))

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
     (fold-right
      (lambda (obj acc)
        (let-values ([(key val)
                      (call-with-values
                       (lambda () (proj obj))
                       (case-lambda
                         [(f)   (values f obj)]
                         [(f g) (values f g)]))])
          (hashtable-update! acc key (cut cons val <>) '())
          acc))
      (make-equal-hashtable)
      objs)))

  (define singleton? (conjoin pair? (compose null? cdr)))

  (define (single-out obj)
    (if (singleton? obj)
        (car obj)
        obj))

  (define (false-map proc . objs)
    (and (for-all values objs)
         (apply proc objs)))

  (define (vector-fold proc init v . vs)
    (define l (vector-length v))
    (unless (for-all (lambda (v^) (equal? (vector-length v^) l)) vs)
      (error 'vector-fold "All vectors must have the same length"))
    (let loop ([acc init] [i 0])
      (if (= i l)
          acc
          (loop (apply proc
                       acc
                       (map (cut vector-ref <> i) (cons v vs)))
                (+ 1 i)))))

  (define (vector-set vec key value)
    (define vec^ (vector-map values vec))
    (vector-set! vec^ key value)
    vec^)

  (define (vector-update vec key fn)
    (vector-set vec key (fn (vector-ref vec key))))

  (define (vector-exists pred vec)
    (let loop ([i 0])
      (and (< i (vector-length vec))
           (or (pred (vector-ref vec i))
               (loop (+ 1 i))))))

  (define (snoc rdc rac) (append rdc (list rac)))
  (define rac last)
  (define (rdc lst) (drop-right lst 1))
  (define (rdc+rac lst)
    (let-values ([(rdc rac.null) (split-at lst (- (length lst) 1))])
      (values rdc (car rac.null))))

  (define (proccall fn . args) (apply fn args))

  (define (zippers xs)
    (let loop ([left '()]
               [right xs])
      (if (null? right)
          (list)
          (cons (list left (car right) (cdr right))
                (loop (cons (car right) left)
                      (cdr right))))))

  (define (copy-object obj)
    (cond
      [(pair? obj) (cons (copy-object (car obj))
                         (copy-object (cdr obj)))]
      [(vector? obj) (vector-map copy-object obj)]
      [else obj]))

  ;; Mutate ~obj~ such that each subterm which is ~eq?~ to ~from~ is now ~to~.
  ;; Returns ~obj~, which is mutated in-place.
  (define (replace-in-object! obj from to)
    (cond
      [(pair? obj)
       (if (eq? (car obj) from)
           (set-car! obj to)
           (replace-in-object! (car obj) from to))
       (if (eq? (cdr obj) from)
           (set-cdr! obj to)
           (replace-in-object! (cdr obj) from to))]
      [(vector? obj)
       (let loop ([i 0])
         (when (< i (vector-length obj))
           (if (eq? (vector-ref obj i) from)
               (vector-set! obj i to)
               (replace-in-object! (vector-ref obj i) from to))
           (loop (+ i 1))))])
    obj)

  (define-syntax-rule (named-lambda name arglist body ...)
    (letrec ([name (lambda arglist body ...)])
      name)))
