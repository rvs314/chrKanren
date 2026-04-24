#!r6rs

(import (rnrs)
        (chrKanren test)
        (chrKanren check)
        (chrKanren generator)
        (chrKanren hashmap)
        (chrKanren utils)
        (srfi :27 random-bits))

(define (pair<? l r)
  (< (car l) (car r)))

(define (sorted-alist alist)
  (sort pair<? alist))

(define (string-generator sz)
  (number->string (- (random-integer (+ 1 (* 2 (+ sz 5))))
                     (+ sz 5))))

(define (int-generator sz)
  (- (random-integer (+ 1 (* 4 (+ sz 5))))
     (* 2 (+ sz 5))))

(define (copyable-value-generator sz)
  ((random-proc
    string-generator
    (compose list string-generator)
    (compose vector string-generator string-generator))
   sz))

(define (kv-generator value-generator)
  (lambda (sz)
    (cons (int-generator sz)
          (value-generator sz))))

(define (alist-generator value-generator)
  (lambda (sz)
    (let loop ([n (random-integer (+ sz 1))])
      (if (zero? n)
          '()
          (cons ((kv-generator value-generator) sz)
                (loop (- n 1)))))))

(define (alist->canonical alist)
  (let loop ([rest alist]
             [acc '()])
    (if (null? rest)
        (sorted-alist acc)
        (let* ([k.v (car rest)]
               [key (car k.v)]
               [acc^ (cons k.v
                           (filter (lambda (entry)
                                     (not (= key (car entry))))
                                   acc))])
          (loop (cdr rest) acc^)))))

(define generic-empty-intmap
  (make-hashmap integer? string? values))

(define-hashmap-type intmap integer? string? values)
(define-hashmap-type any-intmap integer? any? values)

(define-test hashmap-empty
  (check (hashmap-empty? generic-empty-intmap))
  (check (equal? '() (hashmap->alist generic-empty-intmap))))

(define-test (hashmap-set/assoc/ref [entries (alist-generator string-generator)]
                                    [key int-generator]
                                    [value string-generator])
  (define hm0 (alist->hashmap integer? string? values entries))
  (define hm1 (hashmap-set hm0 key value))
  (define expected (alist->canonical (cons (cons key value) entries)))
  (check (equal? value (hashmap-ref hm1 key)))
  (check (equal? (cons key value) (hashmap-assoc hm1 key)))
  (check (equal? expected (sorted-alist (hashmap->alist hm1)))))

(define-test (hashmap-alist/keys/values [entries (alist-generator string-generator)])
  (define hm (alist->hashmap integer? string? values entries))
  (define expected (alist->canonical entries))
  (check (equal? expected
                 (sorted-alist (hashmap->alist hm))))
  (check (equal? (map car expected)
                 (sort < (hashmap-keys hm))))
  (check (equal? (sort string<? (map cdr expected))
                 (sort string<? (hashmap-values hm)))))

(define-test (define-hashmap-type-constructors [entries (alist-generator string-generator)])
  (define hm (alist->intmap entries))
  (define expected (alist->canonical entries))
  (check (intmap? hm))
  (check (equal? expected
                 (sorted-alist (intmap->alist hm)))))

(define-test (define-hashmap-type-copy [entries (alist-generator copyable-value-generator)])
  (define hm0 (alist->any-intmap entries))
  (define hm1 (any-intmap-copy hm0))
  (define al0 (sorted-alist (any-intmap->alist hm0)))
  (define al1 (sorted-alist (any-intmap->alist hm1)))
  (check (equal? al0 al1))
  (check (not (eq? hm0 hm1)))
  (for-each
   (lambda (left right)
     (when (or (pair? (cdr left))
               (vector? (cdr left)))
       (check (not (eq? (cdr left) (cdr right))))))
   al0
   al1))
