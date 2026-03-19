#!r6rs

(library (chrKanren debug)
  (export if-debugging)
  (import (rnrs)
          (chrKanren utils)
          (for (srfi :98 os-environment-variables) expand))

  (define-syntax if-debugging
    (lambda (stx)
      (syntax-case stx ()
        [(if-debugging on-debug)
         #'(if-debugging on-debug 'runtime)]
        [(if-debugging on-debug on-runtime)
         (if (string-ci=? (or (get-environment-variable "DEBUG")
                              "ON")
                          "ON")
             #'on-debug
             #'on-runtime)]))))
