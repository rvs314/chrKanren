#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren check)
        (chrKanren debug))

(puts (if-debugging "DEBUG" "NODEBUG"))

(define-check (foo [x integer?]) string? 3)

(foo "nine")
