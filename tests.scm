#!r6rs

(library (chrKanren tests)
  (export test-check)
  (import (chezscheme))

  (define-syntax test-check
    (syntax-rules ()
      ((_ title tested-expression expected-result)
       (begin
         (printf "Testing ~s~%" 'title)
         (let* ((expected expected-result)
                (produced tested-expression))
           (or (equal? expected produced)
               (errorf 'test-check
                 "Test ~a Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                 'title 'tested-expression expected produced))))))))
