#!r6rs

(library (chrKanren streams)
  (export stream stream?
          make-solution solution solution? solution-first solution-rest
          make-choice   choice   choice?   choice-left choice-right
          make-pause    pause    pause?    pause-state pause-goal
          make-bind     bind     bind?     bind-stream bind-goal
          empty-stream  empty    empty?
          make-singleton singleton?
          mature?)

  (import (rnrs) (chrKanren check))

  (define-record-type stream)

  (define-record-type solution
    (parent stream)
    (fields first rest)
    (protocol
     (lambda (new)
       (lambda (first rest)
         (check (stream? rest) "Solution-rest is a stream")
         (check (not (stream? first)) "Solution is not a stream" first)
         ((new) first rest)))))

  (define-record-type choice
    (parent stream)
    (fields left right)
    (protocol
     (lambda (new)
       (lambda (left right)
         (check (stream? left) "choice-left is a stream")
         (check (stream? right) "choice-right is a stream")
         ((new) left right)))))

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
         ((new) strm gl)))))

  (define-record-type empty
    (parent stream))

  (define empty-stream (make-empty))

  (define (make-singleton obj)
    (make-solution obj empty-stream))

  (define (singleton? obj)
    (and (solution? obj)
         (empty? (solution-rest obj))))

  (define (mature? strm)
    (and (or (empty? strm) (or (solution? strm))))))
