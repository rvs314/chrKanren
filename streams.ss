#!r6rs

(library (chrKanren streams)
  (export stream stream?
          make-solution solution solution? solution-first solution-rest
          make-choice   choice   choice?   choice-left choice-right
          make-pause    pause    pause?    pause-state pause-goal
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
       (lambda (left right)
         (check (stream? left) "choice-left is a stream")
         (check (stream? right) "choice-right is a stream")
         (cond
           [(empty? left)  right]
           [(empty? right) left]
           [else ((new) left right)])))))

  (define-record-type pause
    (parent stream)
    (fields state goal)
    (protocol
     (lambda (new)
       (lambda (st gl)
         (check (not (stream? st)) "pause-state is not a stream" st)
         (check (not (stream? gl)) "pause-goal is not a stream" gl)
         ((new) st gl)))))

  (define-record-type bind
    (parent stream)
    (fields stream goal)
    (protocol
     (lambda (new)
       (lambda (strm gl)
         (check (stream? strm))
         (check (not (stream? gl)) "bind-goal is not a stream" gl)
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
       (lambda (strm)
         (check (stream? strm))
         (if (empty? strm)
             strm
             ((new) strm))))))

  (define (make-singleton state)
    (make-solution state empty-stream))

  (define singleton-solution?
    (conjoin solution? (compose empty? solution-rest)))

  (define mature? (disjoin empty? solution?)))
