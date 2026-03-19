#!r6rs

(library (chrKanren intmaps)
  (export)
  (import (rnrs) (chrKanren utils) (chrKanren check))

  #|
  shift({ Kᵢ → Vᵢ }, B) = { (Kᵢ << r | B) → Vᵢ }

  intmap ::= '()          -- {}
           | (K . V)      -- { K → V }
           | #(X₁ ... Xᵣ) -- ⋃ᵢ shift(Xᵢ, i)
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
      [(vector? im) (apply append (vector->list (vector-map intmap-values im)))])))
