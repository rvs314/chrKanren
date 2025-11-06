#!r6rs

(import (rnrs)
        (chrKanren generator)
        (chrKanren utils)
        (chrKanren interp) (chrKanren streams) (chrKanren goals)
        (chrKanren relation) (chrKanren vars) (chrKanren test)
        (chrKanren state)
        (only (srfi :1 lists) lset=)
        (srfi :64 testing)
        (srfi :39 parameters))

;; Hard mode: Takes ~5 minutes on my machine
#;
(begin
  (*default-random-size* 1000)
  (*default-test-count* 10000))

(define-test bind-is-immature
  (check (not (mature? (make-bind (make-singleton empty-state) succeed)))))

(define-test (bind-left-identity [g goal-generator])
  (check (empty? (mature-finite (make-bind empty-stream g)))))

(define-test (bind-right-zero [r stream-generator])
  (check (empty? (mature-finite (make-bind r fail)))))

(define (result=? l r)
  (lset= (make-tree=? state=?) l r))

(define-test (bind-right-identity [r stream-generator])
  (check (result=?
          (take-finite r)
          (take-finite (make-bind r succeed)))))

(define-test (choice-identity [r stream-generator])
  (check (result=?
          (take-finite r)
          (take-finite (make-choice r empty-stream))))
  (check (result=?
          (take-finite r)
          (take-finite (make-choice empty-stream r)))))

(define-test (disj-commutative [j goal-generator]
                               [k goal-generator]
                               [s state-generator])
  (check (result=? (take-finite (start s (disj j k)))
                   (take-finite (start s (disj k j))))))

(define-test (disj-assoc [i goal-generator]
                         [j goal-generator]
                         [k goal-generator]
                         [s state-generator])
  (check (result=? (take-finite (start s (disj (disj i j) k)))
                   (take-finite (start s (disj i (disj j k)))))))

(define-test (conj-commutative [j goal-generator]
                               [k goal-generator]
                               [s state-generator])
  (check
   (result=?
    (take-finite (start s (conj j k)))
    (take-finite (start s (conj k j))))))

(define-test (conj-assoc [i goal-generator]
                         [j goal-generator]
                         [k goal-generator]
                         [s state-generator])
  (check
   (result=?
    (take-finite (start s (conj (conj i j) k)))
    (take-finite (start s (conj i (conj j k)))))))
