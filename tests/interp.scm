#!r6rs

(import (rnrs)
        (chrKanren interp) (chrKanren streams) (chrKanren goals)
        (chrKanren relation) (chrKanren vars) (chrKanren test)
        (chrKanren state) (chrKanren subst)
        (srfi :64 testing)
        (srfi :39 parameters)
        (srfi :27 random-bits))

(define random-relation-counter 0)

(define (random-relation)
  (define the-body (random-goal))
  (define new-relation
    (make-relation
     (string->symbol
      (string-append "random-relation-" (number->string random-relation-counter)))
     (list)
     (lambda () the-body)))
  (set! random-relation-counter (+ 1 random-relation-counter))
  new-relation)

(define (random-goal)
  (random-case
   [(weight 2) succeed]
   [(weight 2) fail]
   [(disj (random-goal) (random-goal))]
   [(conj (random-goal) (random-goal))]
   [(make-call (random-relation) '())]
   #;[(let ([g (random-goal)]) (with-values () g))]
   [(let ([g (random-goal)]) (Zzz g))]))

(define (random-state)
  (define var-count 10)

  (define vars
    (let loop ([i var-count])
      (if (zero? i)
          '()
          (cons (make-var
                 (random-case
                  ['w]
                  ['x]
                  ['y]
                  ['z]))
                (loop (- i 1))))))

  (define (random-var)
    (list-ref vars (random-integer var-count)))

  (define (random-value)
    (random-case
     ['()]
     [(random-integer 30)]
     [(random-var)]
     [(cons (random-value) (random-value))]))

  (define (random-assoc)
    (random-case
     [empty-subst]
     [(weight 2) (or (extend (random-var) (random-value) (random-assoc))
                     (random-assoc))]))

  (parameterize ([*var-counter* 0])
    (make-state (random-assoc))))

(define (random-stream)
  (random-case
   [empty-stream]
   [(make-solution (random-state) (random-stream))]
   [(make-bind   (random-stream) (random-goal))]
   [(make-choice (random-stream) (random-stream))]))

(define (test-mature-stream name strm)
  (test-assert (string-append name "-mature?") (mature? strm))
  (test-equal  (string-append name "-age") strm (age strm))
  (test-equal  (string-append name "-mature") strm (mature strm)))

(test-begin "interp")

(test-mature-stream "empty-stream" empty-stream)
(test-mature-stream "solution-stream" (make-solution 1 (make-bind empty-stream fail)))

(test-assert "bind is immature" (not (mature? (make-bind empty-stream succeed))))

(test-assert "(make-bind empty-stream r) ≈ empty-stream"
  (empty? (mature (make-bind empty-stream (random-goal)))))

(with-random-test "(make-bind r fail) ≈ empty-stream"
  (test-assert (empty? (mature (make-bind (random-stream) fail)))))

(with-random-test "(make-bind r succeed) ≈ r"
  (let* ((r (random-stream)))
    (test-equal (take r) (take (make-bind r succeed)))))

(with-random-test "(make-bind r succeed) ≈ r"
  (let* ((r (random-stream)))
    (test-equal (take r) (take (make-bind r succeed)))))

(with-random-test "(make-choice r empty-stream) ≈ (make-choice empty-stream r) ≈ r"
  (let* ((r (random-stream)))
    (test-equal "left-identity"
      (take r)
      (take (make-choice r empty-stream)))
    (test-equal "right-identity"
      (take r)
      (take (make-choice empty-stream r)))))

(test-end "interp")
