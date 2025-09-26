#!r6rs

(library (chrKanren test)
  (export check
          define-test test-count
          *test-output-port* *default-test-count*
          *fail-fast*
          *finite-step-count* mature-finite take-finite)
  (import (rnrs)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren relation)
          (chrKanren goals)
          (chrKanren vars)
          (chrKanren interp)
          (chrKanren state)
          (chrKanren subst)
          (chrKanren state)
          (chrKanren streams)
          (srfi :64 testing)
          (srfi :39 parameters))

  (define *test-output-port* (make-parameter (current-error-port)))

  (define *default-test-count*
    (make-parameter 30))

  (define *test-generator-growth-function*
    (make-parameter
     (lambda (input)
       (exact
        (floor
         (expt 2.5 (* input 10)))))))

  (define (test-size input)
    ((*test-generator-growth-function*) input))

  (define-syntax test-count (syntax-rules ()))

  (define (fresh-line str)
    (if (and (positive? (string-length str))
             (char=? (string-ref str (- (string-length str) 1)) #\newline))
        str
        (string-append str (string #\newline))))

  (define-condition-type &test-condition &condition
    make-test-condition test-condition?
    (arg test-condition-args))

  (define *fail-fast* (make-parameter #f))

  (define-syntax define-test
    (syntax-rules (test-count)
      [(define-test (name [nm gen] ...)
         (test-count k)
         body body* ...)
       (begin
         (define (test-body nm ...)
           (guard [e [else
                      (raise
                       (condition
                        e
                        (make-test-condition
                         (map cons '(nm ...)
                              (list nm ...)))))]]
             body body* ...))
         (define (run-test test-cont)
           (define (show . objs)
             (for-each (lambda (o)
                         (when o
                           (display o (*test-output-port*))))
                       objs))
           (show "Running " 'name "...")
           (flush-output-port (*test-output-port*))
           (guard [e [else
                      (show "FAILED: "
                            #\newline
                            (and (message-condition? e)
                                 (fresh-line (condition-message e))))
                      (when (test-condition? e)
                        (for-each (lambda (k.v)
                                    (show (car k.v) " ← " (cdr k.v) #\newline))
                                  (test-condition-args e)))
                      (when (*fail-fast*)
                        (raise e))]]
                (test-cont)
                (show "passed" #\newline)))
         (define (name nm ...)
           (run-test (lambda () (test-body nm ...))))
         (define count k)
         (cond
           [(positive? count)
            (run-test
             (lambda ()
               (let loop ([i 1])
                 (when (<= i count)
                   (let ([nm (gen (test-size (/ i count)))] ...)
                     (test-body nm ...)
                     (loop (+ i 1)))))))]
           [(zero? count)
            (run-test (lambda () 'skipped))]
           [else
            (error 'define-test "test-count must be an integer")]))]
      [(define-test (name)
         body body* ...)
       (define-test (name)
         (test-count 1)
         body body* ...)]
      [(define-test (name arg ...)
         body body* ...)
       (define-test (name arg ...)
         (test-count (*default-test-count*))
         body body* ...)]
      [(define-test name
         body body* ...)
       (define-test (name)
         body body* ...)]))

  (define *finite-step-count* (make-parameter 300))

  (define (mature-finite strm)
    (let ([sm (mature strm (*finite-step-count*))])
      (check (mature? sm) "Stream is immature" strm)
      sm))

  (define (take-finite strm)
    (let loop ([strm strm]
               [idx (*finite-step-count*)]
               [acc '()])
      (let ([strm* (mature-finite strm)])
        (cond
          [(empty? strm*)    (reverse acc)]
          [(zero? idx)       (error 'take-finite "Stream is infinite")]
          [(solution? strm*) (loop (solution-rest strm*)
                                   (- idx 1)
                                   (cons (solution-first strm*) acc))]
          [else (error 'take-finite "Invalid stream")])))))
