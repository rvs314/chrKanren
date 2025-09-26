#!r6rs

(import (rnrs)
        (chrKanren utils)
        (chrKanren check)
        (chrKanren test)
        (chrKanren generator)
        (chrKanren streams)
        (only (srfi :1 lists) list-tabulate first second)
        (srfi :39 parameters))

(define *generated* (make-parameter 100))

(define (test-generator gen . kinds)
  (define generated
    (list-tabulate (*generated*) (lambda (_) (gen))))
  (for-each
   (lambda (kind)
     (check (exists kind generated)
            "All kinds match a generated item"))
   kinds)
  (for-each
   (lambda (gen)
     (check (exists (lambda (k) (k gen)) kinds)
            "All generated items match a kind"
            gen))
   generated))

(define example-generator
  (random-generator
   [() (list)]
   [(x) (list x)]
   [(x y) (list x y)]))

(define-test test-example-generator
  (define (singleton-list? obj)
    (and (= (length obj) 1)
         (<= (first obj) (*default-random-size*))))
  (define (double-list? obj)
    (and (= (length obj) 2)
         (<= (apply + obj) (*default-random-size*))))

  (test-generator example-generator
                  null?
                  singleton-list?
                  double-list?))

(define-test test-stream-generator
  (test-generator stream-generator
                  empty?
                  solution?
                  bind?
                  choice?
                  pause?))
