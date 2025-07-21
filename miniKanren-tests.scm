#!r6rs

(import (rnrs) (chrKanren tests) (chrKanren microKanren) (chrKanren miniKanren))

;; To be moved to a separate miniKanren test file:

(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))

(test-check run*
  (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))


(test-check run*2
  (run* (q x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))

;; TRICKY: Jason has two versions of this
(define (rember*o tr o)
  (conde
   ((== '() tr) (== '() o))
   ((fresh (a d)
      (== `(,a . ,d) tr)
      (conde
        ((fresh (aa da)
           (== `(,aa . ,da) a)
           (fresh (a^ d^)
             (rember*o a a^)
             (rember*o d d^)
             (== `(,a^ . ,d^) o))))
        ((== a 8) (rember*o d o))
        ((fresh (d^)
           (rember*o d d^)
           (== `(,a . ,d^) o))))))))

(test-check rember*o
  (run 8 (q) (rember*o q '(1 2 8 3 4 5)))
  '((1 2 8 3 4 5)
    (1 2 8 3 4 5 8)
    (1 2 8 3 4 8 5)
    (1 2 8 3 8 4 5)
    (1 2 8 8 3 4 5)
    (1 2 8 8 3 4 5)
    (1 8 2 8 3 4 5)
    (8 1 2 8 3 4 5)))

;; TODO: The following test is broken, we apparently use
;; a different execution order. Why?

#;
(test-check rember*o
  (run 9 (q) (rember*o q '(1 x 5)))
  '((1 x 5)
    (1 x 5 8)
    (1 x 5 8 8)
    (1 x 8 5)
    (1 8 x 5)
    (8 1 x 5)
    (1 x 5 8 8 8)
    (1 x 5 8 8 8 8)
    (1 x 5 8 8 8 8 8)))

#;
((1 x 5)
 (1 x 5 8)
 (1 x 8 5)
 (1 8 x 5)
 (8 1 x 5)
 (1 x 5 8 8)
 (1 x 8 5 8)
 (1 8 x 5 8)
 (8 1 x 5 8))
