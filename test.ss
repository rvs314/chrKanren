#!r6rs

(library (chrKanren test)
  (export check
          define-test test-count
          *default-test-count*
          *fail-fast*
          *finite-step-count*
          mature-finite take-finite
          run-finite run*-finite)
  (import (rnrs)
          (chrKanren check)
          (chrKanren syntax)
          (chrKanren utils)
          (chrKanren relation)
          (chrKanren goals)
          (chrKanren vars)
          (chrKanren reifier)
          (chrKanren interp)
          (chrKanren state)
          (chrKanren streams)
          (srfi :39 parameters))

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

  (define *fail-fast* (make-parameter (member "--fail-fast" (cdr (command-line)))))

  (define (report . objs)
    (let-values (((op objs)
                  (if (and (pair? objs) (output-port? (car objs)))
                      (car+cdr objs)
                      (values (current-output-port) objs))))
      (for-each (lambda (o)
                  (when o
                    (display o op)))
                objs)
      (flush-output-port op)))

  (define (report-condition e)
    (report (current-error-port)
            "FAILED: "
            #\newline
            (and (message-condition? e)
                 (fresh-line (condition-message e))))
    (when (test-condition? e)
      (for-each (lambda (k.v)
                  (report (current-error-port)
                          (car k.v) " ← " (cdr k.v) #\newline))
                (test-condition-args e)))
    (when (*fail-fast*)
      (raise e)))

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
           (report "Running " 'name "...")
           (guard [e [else (report-condition e)]]
             (test-cont)
             (report "passed" #\newline)))
         (define (name nm ...)
           (run-test (lambda () (test-body nm ...))))
         (define count k)
         (cond
           [(positive? count)
            (run-test
             (lambda ()
               (repeatedly
                count
                (lambda (i)
                  (let ([nm (gen (test-size (/ i count)))] ...)
                    (test-body nm ...))))))]
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

  (define take-finite
    (case-lambda
      [(strm) (take-finite +inf.0 strm)]
      [(cnt strm)
       (let loop
           ([strm strm]
            [cnt cnt]
            [acc '()])
         (let ([strm* (mature-finite strm)])
           (cond
             [(or (zero? cnt) (empty? strm*))
              (reverse acc)]
             [(solution? strm*)
              (loop (solution-rest strm*)
                    (- cnt 1)
                    (cons (solution-first strm*) acc))]
             [else
              (error 'take-finite "Invalid stream")])))]))

  (define-syntax-rule (run-finite amt (var ...) goal ...)
    (parameterize ([*var-counter* 0])
      (fresh (var ...)
        (let ([vs (list var ...)]
              [rs (take-finite amt
                               (start empty-state (conj goal ...)))])
          (map (lambda (r) (reify (reify-query vs r))) rs)))))

  (define-syntax-rule (run*-finite (var ...) goal ...)
    (run-finite +inf.0 (var ...) goal ...)))
