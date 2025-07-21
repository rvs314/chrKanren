#!r6rs

(import (rnrs)
        (chrKanren microKanren)
        (chrKanren miniKanren)
        (chrKanren tests))

;;; Test programs

(define a-and-b
  (conj
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh
    (lambda (b)
      (disj
       (== b 5)
       (== b 6))))))

(define fives
  (lambda (x)
    (disj
     (== x 5)
     (lambda (a/c)
       (lambda ()
         ((fives x) a/c))))))

(define appendo
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (== `(,a . ,res) out)
                (lambda (s/c)
                  (lambda ()
                    ((appendo d s res) s/c))))))))))))))

(define appendo2
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (lambda (s/c)
                  (lambda ()
                    ((appendo2 d s res) s/c)))
                (== `(,a . ,res) out))))))))))))

(define call-appendo
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (appendo l s out)
               (== `(,l ,s ,out) q)))))))))))


(define call-appendo2
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (appendo2 l s out)
               (== `(,l ,s ,out) q)))))))))))

(define call-appendo3
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (== `(,l ,s ,out) q)
               (appendo l s out)))))))))))

(define ground-appendo (appendo '(a) '(b) '(a b)))

(define ground-appendo2  (appendo2 '(a) '(b) '(a b)))

(define relo
  (lambda (x)
    (call/fresh
     (lambda (x1)
       (call/fresh
        (lambda (x2)
          (conj
           (== x `(,x1 . ,x2))
           (disj
            (== x1 x2)
            (lambda (s/c)
              (lambda () ((relo x) s/c)))))))))))

(define many-non-ans
  (call/fresh
   (lambda (x)
     (disj
      (relo `(5 . 6))
      (== x 3)))))

;; Test cases

(test-check "second-set t1"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (state-assoc (car $)))
  `((,(var 0) . 5)))

(test-check "second-set t2"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (a-and-b empty-state)))
    (state-assoc (car $)))
  `((,(var 1) . 5) (,(var 0) . 7)))

(test-check "second-set t3, take"
  (let (($ (a-and-b empty-state)))
    (map state-assoc (take 1 $)))
  `(((,(var 1) . 5) (,(var 0) . 7))))

(test-check "second-set t4"
  (let (($ (a-and-b empty-state)))
    (state-assoc (car (cdr $))))
  `((,(var 1) . 6) (,(var 0) . 7)))

(test-check "second-set t5"
  (let (($ (a-and-b empty-state)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
    (map state-assoc (take 1 $)))
  `(((,(var 0) . 5))))

(test-check "take 2 a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map state-assoc (take 2 $)))
  `(((,(var 1) . 5) (,(var 0) . 7))
    ((,(var 1) . 6) (,(var 0) . 7))))

(test-check "take-all a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (map state-assoc (take-all $)))
  `(((,(var 1) . 5) (,(var 0) . 7))
    ((,(var 1) . 6) (,(var 0) . 7))))

(test-check "ground appendo"
  (state-assoc (car ((ground-appendo empty-state))))
  `((,(var 2) b) (,(var 1)) (,(var 0) . a)))

(test-check "ground appendo2"
  (state-assoc (car ((ground-appendo2 empty-state))))
  `((,(var 2) b) (,(var 1)) (,(var 0) . a)))

(test-check "appendo"
  (map state-assoc (take 2 (call-appendo empty-state)))
  `(((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 2) . ,(var 3)) (,(var 1)))
    ((,(var 0) ,(var 1) ,(var 2) ,(var 3))
     (,(var 2) . ,(var 6))
     (,(var 5))
     (,(var 3) ,(var 4) . ,(var 6))
     (,(var 1) ,(var 4) . ,(var 5)))))

(test-check "appendo2"
  (map state-assoc (take 2 (call-appendo2 empty-state)))
  `(((,(var 0) ,(var 1) ,(var 2) ,(var 3))
     (,(var 2) . ,(var 3))
     (,(var 1)))
    ((,(var 0) ,(var 1) ,(var 2) ,(var 3))
     (,(var 3) ,(var 4) . ,(var 6))
     (,(var 2) . ,(var 6))
     (,(var 5))
     (,(var 1) ,(var 4) . ,(var 5)))))

(test-check "reify-1st across appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "reify-1st across appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "many non-ans"
  (map state-assoc (take 1 (many-non-ans empty-state)))
  `(((,(var 0) . 3))))
