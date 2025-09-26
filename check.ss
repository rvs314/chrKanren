#!r6rs

(library (chrKanren check)
  (export check
          &failed-check make-failed-check failed-check?
          failed-check-results)
  (import (rnrs))

  (define-condition-type &failed-check &violation
    make-failed-check failed-check?
    (results failed-check-results))

  (define (fail-check who context alist)
    (define (print-assoc out)
      (lambda (l.r)
        (write (car l.r) out)
        (display " → " out)
        (write (cdr l.r) out)
        (newline out)))
    (define (print-error-message out)
      (display "Check Failed" out)
      (when who
        (display " (" out)
        (display who out)
        (display ")" out))
      (display ": " out)
      (write context out)
      (newline out)
      (for-each (print-assoc out) alist))
    (condition
     (if who
         (make-who-condition who)
         (condition))
     (make-message-condition
      (call-with-string-output-port print-error-message))
     (make-failed-check alist)))

  (define-syntax check
    (syntax-rules ()
      [(check (op arg ...) msg more ...)
       (let ([operator op]
             [arg-lst  (list arg ...)])
         (unless (apply operator arg-lst)
           (let ([alist
                  (map cons
                       `(op        arg ... more ...)
                       `(,operator ,@arg-lst ,more ...))])
             (raise-continuable
              (fail-check msg '(op arg ...) alist)))))]
      [(check #t _) (begin)]
      [(check #f msg more ...)
       (raise-continuable
        (fail-check msg 'impossible `((more . ,more) ...)))]
      [(check fact msg more ...)
       (let ([fact-value fact])
         (unless fact-value
           (raise-continuable
            (fail-check msg 'fact `((fact . ,fact-value) (more . ,more) ...)))))]
      [(check we)
       (check we #f)])))
