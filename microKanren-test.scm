#!r6rs

(import (rnrs) (chrKanren microKanren) (chrKanren miniKanren) (only (chezscheme) load))
(load "microKanren-test-programs.scm")

;;; Test programs

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (display "Testing ")
       (display title)
       (newline)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (error 'test-check
                    "Test-check failed"
                    'tested-expression
                    expected
                    produced)))))))

(define (appendo l s out)
  (conde
    ((== '() l) (== s out))
    ((fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res)))))

#;
(test-check 'run*
  (run* (q) (fresh (x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5))))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))

#;
(test-check 'run*2
  (run* (q x y) (== `(,x ,y) q) (appendo x y '(1 2 3 4 5)))
  '((() (1 2 3 4 5))
    ((1) (2 3 4 5))
    ((1 2) (3 4 5))
    ((1 2 3) (4 5))
    ((1 2 3 4) (5))
    ((1 2 3 4 5) ())))

#;
(test-check 'rember*o
  (letrec
      ((rember*o (lambda (tr o)
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
                             (== `(,a . ,d^) o))))))))))
      (run 8 (q) (rember*o q '(1 2 8 3 4 5))))
  '((1 2 8 3 4 5)
    (1 2 8 3 4 5 8)
    (1 2 8 3 4 8 5)
    (1 2 8 3 8 4 5)
    (1 2 8 8 3 4 5)
    (1 2 8 8 3 4 5)
    (1 8 2 8 3 4 5)
    (8 1 2 8 3 4 5)))

#;
(test-check 'rember*o
  (letrec
      ((rember*o (lambda (tr o)
                   (conde
                     ((== '() tr) (== '() o))
                     ((fresh (a d)
                        (== `(,a . ,d) tr)
                        (conde
                          ((fresh (aa da)
                             (== `(,aa . ,da) a)
                             (fresh (a^ d^)
                               (== `(,a^ . ,d^) o)
                               (rember*o d d^)
                               (rember*o a a^))))
                          ((== a 8) (rember*o d o))
                          ((fresh (d^)
                             (== `(,a . ,d^) o)
                             (rember*o d d^))))))))))
      (run 9 (q) (rember*o q '(1 (2 8 3 4) 5))))
  '((1 (2 8 3 4) 5)
    (1 (2 8 3 4) 5 8)
    (1 (2 8 3 4) 5 8 8)
    (1 (2 8 3 4) 8 5)
    (1 8 (2 8 3 4) 5)
    (8 1 (2 8 3 4) 5)
    (1 (2 8 3 4) 5 8 8 8)
    (1 (2 8 3 4) 5 8 8 8 8)
    (1 (2 8 3 4) 5 8 8 8 8 8)))

(test-check "second-set t1"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (state-assoc (car $)))
  '((#(0) . 5)))

(test-check "second-set t2"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (a-and-b empty-state)))
    (state-assoc (car $)))
  '((#(1) . 5) (#(0) . 7)))

(test-check "second-set t3, take"
  (let (($ (a-and-b empty-state)))
    (map state-assoc (take 1 $)))
  '(((#(1) . 5) (#(0) . 7))))

(test-check "second-set t4"
  (let (($ (a-and-b empty-state)))
    (state-assoc (car (cdr $))))
  '((#(1) . 6) (#(0) . 7)))

(test-check "second-set t5"
  (let (($ (a-and-b empty-state)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
    (map state-assoc (take 1 $)))
  '(((#(0) . 5))))

(test-check "take 2 a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map state-assoc (take 2 $)))
  '(((#(1) . 5) (#(0) . 7))
    ((#(1) . 6) (#(0) . 7))))

(test-check "take-all a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map state-assoc (take-all $)))
  '(((#(1) . 5) (#(0) . 7))
    ((#(1) . 6) (#(0) . 7))))

;; (test-check "ground appendo"
;;   (take-all (ground-appendo empty-state))
;;   '((#(2) b) (#(1)) (#(0) . a)))

;; (test-check "ground appendo2"
;;   (state-assoc (car ((ground-appendo2 empty-state))))
;;   '((#(2) b) (#(1)) (#(0) . a)))

;; (test-check "appendo"
;;   (map state-assoc (take 2 (call-appendo empty-state)))
;;   '(((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1)))
;;     ((#(0) #(1) #(2) #(3)) (#(2) . #(6)) (#(5)) (#(3) #(4) . #(6)) (#(1) #(4) . #(5)))))

;; (test-check "appendo2"
;;   (map state-assoc (take 2 (call-appendo2 empty-state)))
;;   '(((#(0) #(1) #(2) #(3)) (#(3) . #(2)) (#(1))) ((#(0) #(1) #(2) #(3)) (#(3) #(4) . #(6)) (#(6) . #(2)) (#(5)) (#(1) #(4) . #(5)))))

;; (test-check "reify-1st across appendo"
;;   (map reify-1st (take 2 (call-appendo empty-state)))
;;   '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

;; (test-check "reify-1st across appendo2"
;;   (map reify-1st (take 2 (call-appendo2 empty-state)))
;;   '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

;; (test-check "many non-ans"
;;   (map state-assoc (take 1 (many-non-ans empty-state)))
;;   '(((#(0) . 3))))
