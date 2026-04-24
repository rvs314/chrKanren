#!r6rs

(library (chrKanren hashmap)
  (export hashmap?
          (rename [%make-hashmap make-hashmap])
          hashmap-of 
          hashmap-domain hashmap-range
          hashmap-keys hashmap-values hashmap->alist
          alist->hashmap
          hashmap-assoc hashmap-ref hashmap-set
          hashmap-empty?
          define-hashmap-type)
  (import (rnrs)
          (srfi :26 cut)
          (chrKanren utils)
          (chrKanren check)
          (for (chrKanren utils) expand))

  #|
  This is a generic mapping type between a hashable key type and an
  arbitrary value type. It uses an N-way radix trie similar to that
  of faster-miniKanren.
  
  The initial original trie version was due to Abdulaziz Ghuloum.
  Greg Rosenblatt changed it to an N-way Trie to reduce depth.

  This library defines two datatypes: `%hashmap`, which is a
  "datatype" made of Scheme primitive types (pairs, vectors, etc)
  which implement the actual trie; and `hashmap`, which is a
  record type that keeps both a `%hashmap` and a bit of metadata
  about the domain and range of the mapping for contract checking and
  the actual function we use for hashing. A `%hashmap` can only store
  non #f values and assumes fixnums as hashes.
    
  We define `%hashmap` this way:

  K ∈ hashmap-domain, V ∈ hashmap-range
  H ::= '()          -- If H = ∅
      | (K . V)      -- If H = {K → V}
      | #(X₀ ... Xᵣ) -- If H = ⋃ Xᵢ (at least two Xᵢ must be nonempty)
  |#

  (define shift-size 4)
  (define branch-size (fxarithmetic-shift-left 1 shift-size))
  (define empty-branch (make-vector branch-size '()))

  (define (split fx)
    (values (fxarithmetic-shift-right fx shift-size)
            (fxbit-field fx 0 shift-size)))

  #;(define (join hi lo)
    (fxior (fxarithmetic-shift-left hi shift-size) lo))

  ;; Hashmap -> Fixnum -> Value
  (define (%hashmap-ref hm key)
    (cond
      [(and (pair? hm) (fx=? (car hm) key)) hm]
      [(vector? hm)
       (let-values ([(hi lo) (split key)])
         (%hashmap-ref (vector-ref hm lo) hi))]
      [else #f]))

  ;; Hashmap -> Fixnum -> Value -> Hashmap
  (define (%hashmap-set hm hash elements)
    (cond
      [(vector? hm)
       (let-values ([(hi lo) (split hash)])
         (vector-update hm lo (cut %hashmap-set <> hi elements)))]
      [(and (pair? hm) (fx=? (car hm) hash))
       (cons hash elements)]
      [(pair? hm)
       (let* ([hm (%hashmap-set empty-branch (car hm) (cdr hm))]
              [hm (%hashmap-set hm hash elements)])
         hm)]
      [(null? hm)
       (cons hash elements)]))

  (define (%hashmap->alist hm)
    (cond
      [(null? hm) '()]
      [(pair? hm) (list hm)]
      [(vector? hm)
       (apply append
              (vector->list (vector-map %hashmap->alist hm)))]))

  (define (alist->%hashmap hash al)
    (fold-left (lambda (acc k.v)
                 (%hashmap-set acc
                               (hash (car k.v))
                               k.v))
               '()
               al))

  (define (%hashmap-of key-contract value-contract)
    (lambda (obj)
      (cond
        [(null? obj)   #t]
        [(pair? obj)
         (and (fixnum? (car obj))
              (pair? (cdr obj))
              (key-contract (cadr obj))
              (value-contract (cddr obj)))]
        [(vector? obj) (vector-for-all
                        (%hashmap-of key-contract value-contract)
                        obj)]
        [else          #f])))

  (define empty-%hashmap '())

  (define %make-hashmap
    (case-lambda
      [()
       (%make-hashmap any?)]
      [(domain?)
       (%make-hashmap domain? any?)]
      [(domain? range?)
       (%make-hashmap domain? range? equal-hash)]
      [(domain? range? hash)
       (make-hashmap domain? range? hash empty-%hashmap)]))

  (define-record-type hashmap
    (fields domain range hashproc contents))

  (define-check (hashmap-hash [hm hashmap?]
                              [obj (hashmap-domain hm)])
    fixnum?
    ((hashmap-hashproc hm) obj))

  (define-check (hashmap-of [domain range procedure?])
    procedure?
    (lambda (obj)
      (and (hashmap? obj)
           (or (and (predicate-implies? (hashmap-domain obj) domain)
                    (predicate-implies? (hashmap-range obj)  range))
               ((%hashmap-of domain range)
                (hashmap-contents obj))))))

  (define-check (hashmap-keys [hm hashmap?])
    (listof (hashmap-domain hm))
    (map car (hashmap->alist hm)))

  (define-check (hashmap-values [hm hashmap?])
    (listof (hashmap-range hm))
    (map cdr (hashmap->alist hm)))

  (define-check (hashmap->alist [hm hashmap?])
    (listof (pairof (hashmap-domain hm) (hashmap-range hm)))
    (map cdr (%hashmap->alist (hashmap-contents hm))))

  (define-check (hashmap-map-contents [hm hashmap?]
                                      [proc procedure?])
    (hashmap-of (hashmap-domain hm) (hashmap-range hm))
    (make-hashmap (hashmap-domain hm)
                  (hashmap-range hm)
                  (hashmap-hashproc hm)
                  (proc (hashmap-contents hm))))

  (define-check (alist->hashmap [dom rng hash procedure?]
                                [alist (listof (pairof dom rng))])
    (hashmap-of dom rng)
    (make-hashmap dom
                  rng
                  hash
                  (alist->%hashmap hash alist)))

  (define-check (hashmap-set [hm hashmap?]
                             [key (hashmap-domain hm)]
                             [value (hashmap-range hm)])
    (hashmap-of (hashmap-domain hm) (hashmap-range hm))
    (define hashcode (hashmap-hash hm key))
    (hashmap-map-contents hm
                          (lambda (contents)
                            (%hashmap-set contents
                                          hashcode
                                          (cons key value)))))

  (define-check (hashmap-assoc [hm  hashmap?]
                              [key (hashmap-domain hm)])
    (disjoin not (pairof (hashmap-domain hm) (hashmap-range hm)))
    (false-map cdr
               (%hashmap-ref (hashmap-contents hm)
                             (hashmap-hash hm key))))

  (define hashmap-ref
    (case-lambda
      [(hm key)
       (debug-check (hashmap? hm) '(argument hm of hashmap-ref))
       (debug-check ((hashmap-domain hm) key) '(argument key of hashmap-ref))
       (hashmap-ref hm
                    key
                    (lambda ()
                      (assertion-violation 'hashmap-ref
                                           "Hashmap reference failed"
                                           hm
                                           key)))]
      [(hm key on-missing)
       (debug-check (hashmap? hm) '(argument hm of hashmap-ref))
       (debug-check ((hashmap-domain hm) key) '(argument key of hashmap-ref))
       (debug-check (procedure? on-missing)
                    '(argument on-missing of hashmap-ref))
       (let ([res (hashmap-assoc hm key)])
         (if res
             (cdr res)
             (on-missing)))]))

  (define-check (hashmap-empty? [hm hashmap?])
    any?
    (eq? (hashmap-contents hm) empty-%hashmap))

  (define-syntax define-hashmap-type
    (lambda (stx)
      (syntax-case stx ()
        [(define-hashmap-type type-name constructor-arg ...)
         (with-syntax ([empty-map  (identifier 'empty- #'type-name)]
                       [map?       (identifier #'type-name '?)]
                       [map->alist (identifier #'type-name '->alist)]
                       [alist->map (identifier 'alist-> #'type-name)]
                       [copy-map   (identifier #'type-name "-copy")])
           #`(begin
               (define empty-map
                 (%make-hashmap constructor-arg ...))
               (define map?
                 (hashmap-of (hashmap-domain empty-map)
                             (hashmap-range empty-map)))
               (define-check (map->alist [hm map?])
                 (listof (pairof (hashmap-domain empty-map)
                                 (hashmap-range empty-map)))
                 (hashmap->alist hm))
               (define-check (alist->map [al (listof (pairof (hashmap-domain empty-map)
                                                             (hashmap-range empty-map)))])
                 map?
                 (alist->hashmap (hashmap-domain empty-map)
                                 (hashmap-range empty-map)
                                 (hashmap-hashproc empty-map)
                                 al))
               (define-check (copy-map [hm map?])
                 map?
                 (alist->map
                  (map (lambda (k.v)
                         (cons (car k.v)
                               (copy-object (cdr k.v))))
                       (map->alist hm))))))]))))
