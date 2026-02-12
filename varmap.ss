#!r6rs

(library (chrKanren varmap)
  (export empty-varmap
          varmap
          varmap?
          varmap-lookup
          varmap-extend
          varmap-extend-all
          varmap->alist alist->varmap
          varmap-copy)
  (import (rnrs) (chrKanren check) (chrKanren vars) (chrKanren utils))

  #|
  This library implements "varmaps": finite mappings from variables to arbitrary values.
  It uses a variant of the trie in faster-miniKanren's `mk-vicare.scm` implementation.
  Varmaps are just wrappers over intmaps, which map nonnegative fixnums to arbitrary values.

  shift({ Kᵢ → Vᵢ }, B) = { (Kᵢ << r | B) → Vᵢ }

  intmap ::= '()          -- {}
           | (K . V)      -- { K → V }
           | #(X₁ ... Xᵣ) -- ⋃ᵢ shift(Xᵢ, i)

  We use primitive types for intmaps to avoid having to unbox when not needed,
  but wrap the intmaps in varmaps for type-safety
  |#

  (define shift-size 4)
  (define branch-size (fxarithmetic-shift-left 1 shift-size))
  (define empty-branch (make-vector branch-size '()))

  (define (split fx)
    (values (fxarithmetic-shift-right fx shift-size)
            (fxbit-field fx 0 shift-size)))

  (define (join hi lo)
    (fxior (fxarithmetic-shift-left hi shift-size) lo))


  (define not-found (list 'not-found))
  (define (not-found? obj) (eq? obj not-found))

  ;; Intmap -> Fixnum -> Object
  (define (intmap-ref im key)
    (cond
      [(and (pair? im) (fx=? (car im) key))
       (cdr im)]
      [(vector? im)
       (let-values ([(hi lo) (split key)])
         (intmap-ref (vector-ref im lo) hi))]
      [else
       not-found]))

  ;; Intmap -> Fixnum -> Value -> Intmap
  (define (intmap-set im key value)
    (cond
      [(vector? im)
       (let-values ([(hi lo) (split key)])
         (vector-update im lo (lambda (im^) (intmap-set im^ hi value))))]
      [(and (pair? im) (fx=? (car im) key))
       (cons key value)]
      [(pair? im)
       (let* ([im (intmap-set empty-branch (car im) (cdr im))]
              [im (intmap-set im key value)])
         im)]
      [(null? im)
       (cons key value)]))

  ;; Intmap -> List
  (define (intmap->alist im)
    (cond
      [(null? im)   im]
      [(pair? im)   (list im)]
      [(vector? im)
       (let loop ([i 0] [acc '()])
         (if (fx=? i (vector-length im))
             acc
             (loop (+ i 1)
                   (append (map (lambda (x) (cons (join (car x) i) (cdr x)))
                                (intmap->alist (vector-ref im i)))
                           acc))))]))

  (define (intmap-values im)
    (cond
      [(null? im) '()]
      [(pair? im) (list (cdr im))]
      [(vector? im) (apply append (vector->list (vector-map intmap-values im)))]))

  (define-record-type varmap (fields contents))

  (define empty-varmap (make-varmap '()))

  (define-check (varmap-lookup [key var?] [vmap varmap?])
    any?
    (define res (intmap-ref (varmap-contents vmap) (var-idx key)))
    (if (not-found? res)
        key
        (cdr res)))

  (define-check (varmap-extend [key var?] [value any?] [vmap varmap?])
    varmap?
    (define imap  (varmap-contents vmap))
    (define imap^ (intmap-set imap (var-idx key) (cons key value)))
    (define vmap^ (make-varmap imap^))
    vmap^)

  (define alist? (listof (pairof var? any?)))

  (define-check (varmap-extend-all [alist alist?]
                                   [vmap varmap?])
    varmap?
    (fold-left (lambda (acc k.v) (varmap-extend (car k.v) (cdr k.v) acc)) vmap alist))

  (define-check (varmap->alist [vm varmap?]) alist?
    (intmap-values (varmap-contents vm)))

  (define-check (alist->varmap [alist alist?]) varmap?
    (varmap-extend-all alist empty-varmap))

  (define-check (varmap-copy [vm varmap?]) varmap?
    (alist->varmap
     (map (lambda (k.v)
            (cons (car k.v) (copy-object (cdr k.v))))
          (varmap->alist vm)))))
