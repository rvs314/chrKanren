#!r6rs

(import (rnrs)
        (chrKanren check)
        (chrKanren test)
        (chrKanren utils)
        (only (srfi :1 lists)
              list-tabulate pair-fold pair-fold-right
              concatenate)
        (srfi :39 parameters)
        (srfi :27 random-bits)
        (srfi :26 cut))


(define-syntax-rule (foo bar baz)
  (list bar bar))

(define-test (define-syntax-rule)
  (check (equal? (call-with-string-output-port
                  (lambda (op)
                    (foo (display "1" op) (display "2" op))))
                 "11")))

(define-test (test-TODO)
  (guard [e [#t (check (error? e))]]
    TODO
    (check #f)))

(define-test (test-puts)
  (check (equal?
          (call-with-string-output-port
           (lambda (op)
             (parameterize ([*puts-output-port* op])
               (puts 1 "two" 'three))))
          "1 \"two\" three\n")))

(define-test (test-car+cdr)
  (let*-values ([(k) (cons 'car 'cdr)]
                [(a d) (car+cdr k)])
    (check (eq? a 'car))
    (check (eq? d 'cdr))))

(define-test (finding-and-removing)
  (let-values ([(k e) (find-and-remove even? '(1 2 3 4))])
    (check (eqv? k 2))
    (check (equal? e '(1 3 4))))
  (let-values ([(k e) (find-and-remove even? '(1 3 5))])
    (check (not k))
    (check (not k))))

(define-test conjoining
  (check (equal? #t ((conjoin number? even?) 4)))
  (check (equal? #t ((conjoin pair? (compose even? car) (compose odd? cdr)) '(4 . 3))))
  (check (equal? #f ((conjoin list? vector?) 9))))

(define-test composing
  (check (equal?  9 ((compose) 9)))
  (check (equal? '#((#t))
                 ((compose vector list even? (cut + 1 <> )) 3)))
  (check (equal? 9
                 ((compose + car+cdr cons) 4 5))))


(define-test disjoining
  (check (equal? #t ((disjoin number? string?) 4)))
  (check (equal? #t ((disjoin number? string?) "five")))
  (check (equal? #t ((disjoin pair? even?) '(4 . 3))))
  (check (equal? #f ((disjoin list? vector? char? boolean?) 9)))
  (check (equal? #f ((disjoin > =) 3 9)))
  (check (equal? #t ((disjoin < =) 3 9)))
  (check (equal? #t ((disjoin < =) 3 3))))

(define-test consting
  (let-values ([xs ((const 1 2))]
               [ys ((const))]
               [zs ((const 1))])
    (check (equal? xs '(1 2)))
    (check (equal? ys '()))
    (check (equal? zs '(1)))))

(define (sorted? lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (<= (car lst) (cadr lst))
           (sorted? (cdr lst)))))

(define (random-int-list sz)
  (list-tabulate sz
                 (lambda (_) (- (random-integer 60) 30))))

(define-test (sorting [r random-int-list])
  (check (sorted? (sort r))))

(define-test onning
  (check (equal? #t ((on = string-length) "one" "two")))
  (check (equal? #f ((on = string-length) "one" "three")))
  (check (equal? #t ((on < string-length) "one" "three")))
  (check (equal? 8 ((on + length) '(o n e) '(t h r e e)))))

(define abs-tree=? (make-tree=? (on = abs)))

(define-test basic-tree=?
  (check (abs-tree=? '(1 2 (3)) '(-1 2 (-3)))))

(define-test test-memoize
  (define the-table (make-eqv-hashtable))
  (define/memoized (fib n)
    (using the-table)
    (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1)))))
  (check (= (fib 3000)
            410615886307971260333568378719267105220125108637369252408885430926905584274113403731330491660850044560830036835706942274588569362145476502674373045446852160486606292497360503469773453733196887405847255290082049086907512622059054542195889758031109222670849274793859539133318371244795543147611073276240066737934085191731810993201706776838934766764778739502174470268627820918553842225858306408301661862900358266857238210235802504351951472997919676524004784236376453347268364152648346245840573214241419937917242918602639810097866942392015404620153818671425739835074851396421139982713640679581178458198658692285968043243656709796000))
  (check (= (hashtable-ref the-table 297 #f) (fib 297))))
