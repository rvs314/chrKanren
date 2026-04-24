#!r6rs

(library (chrKanren streams)
  (export stream stream?
          make-solution solution solution? solution-first
          solution-rest
          make-choice   choice   choice?   choice-left choice-right
          make-paused-step  paused-step paused-step? paused-step-state
          paused-step-goal
          make-paused-propagation paused-propagation?
          paused-propagation-state
          make-bind     bind     bind?     bind-stream bind-goal
          empty-stream  empty    empty?
          make-propagating propagating propagating? propagating-stream
          make-singleton singleton-solution?
          mature?)

  (import (rnrs) (chrKanren check) (chrKanren utils))

  (define-record-type stream)

  (define-record-type solution
    (parent stream)
    (fields first rest)
    (protocol
     (lambda (new)
       (define-check (make-solution [first (negate stream?)]
                                    [rest stream?])
         solution?
         ((new) first rest))
       make-solution)))

  (define-record-type choice
    (parent stream)
    (fields left right)
    (protocol
     (lambda (new)
       (lambda-check ([left stream?] [right stream?])
         stream?
         (cond
           [(empty? left)  right]
           [(empty? right) left]
           [else ((new) left right)])))))

  (define-record-type paused-step
    (parent stream)
    (fields state goal)
    (protocol
     (lambda (new)
       (lambda-check ([st (negate stream?)]
                      [gl (negate stream?)])
         paused-step?
         ((new) st gl)))))

  (define-record-type paused-propagation
    (parent stream)
    (fields state)
    (protocol
     (lambda (new)
       (lambda-check ([st (negate stream?)])
         paused-propagation?
         ((new) st)))))

  (define-record-type bind
    (parent stream)
    (fields stream goal)
    (protocol
     (lambda (new)
       (lambda-check ([strm stream?] [gl (negate stream?)])
         stream?
         (if (empty? strm)
             strm
             ((new) strm gl))))))

  (define-record-type empty
    (parent stream))

  (define empty-stream (make-empty))

  (define-record-type propagating
    (parent stream)
    (fields stream)
    (protocol
     (lambda (new)
       (named-lambda-check (make-propagating [strm stream?])
         stream?
         (if (empty? strm)
             strm
             ((new) strm))))))

  (define (make-singleton state)
    (make-solution state empty-stream))

  (define singleton-solution?
    (conjoin solution? (compose empty? solution-rest)))

  (define mature? (disjoin* empty? solution?)))
