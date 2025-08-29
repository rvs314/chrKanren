#!r6rs

(library (chrKanren stream)
  (export stream stream?
          make-solution solution solution? solution-first solution-rest
          make-choice   choice   choice?   choice-left choice-right
          make-pause    pause    pause?    pause-state pause-goal
          make-bind     bind     bind?     bind-state bind-goal
          empty-stream  empty    empty?)

  (import (rnrs))

  (define-record-type stream)

  (define-record-type solution
    (parent stream)
    (fields first rest))

  (define-record-type choice
    (parent stream)
    (fields left right))

  (define-record-type pause
    (parent stream)
    (fields state goal))

  (define-record-type bind
    (parent stream)
    (fields state goal))

  (define-record-type empty
    (parent stream))

  (define empty-stream (make-empty)))
