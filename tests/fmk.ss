#!r6rs

(import (rnrs) (rnrs eval))

(define (read-all in)
  (define r (read in))
  (if (eof-object? r) '() (cons r (read-all in))))

(define (read-as-expression in)
  `(let () ,(read-all in)))

(define (run-fmk-test filename)
  (define expr (call-with-input-file filename read-as-expression))
  (eval expr (environment '(rnrs) '(chrKanren base) '(chrKanren tests fmk shim))))
