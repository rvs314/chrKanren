#!r6rs

(import (rnrs)
        (except (chrKanren base) run run*)
        (only (srfi :1 lists) lset<=)
        (chrKanren test)
        (chrKanren vars)
        (chrKanren state)
        (chrKanren interp)
        (chrKanren tests fmk shim)
        (chrKanren utils))

;; This requires more work, so let's pump up the execution limit

(*finite-maturation-limit* 30000)

(defrel (!- exp env t)
  (conde
   [(symbolo exp) (lookupo exp env t)]
   [(fresh (x e t-x t-e)
      (== `(lambda (,x) ,e) exp)
      (symbolo x)
      (not-in-envo 'lambda env)
      (== `(-> ,t-x ,t-e) t)
      (!- e `((,x . ,t-x) . ,env) t-e))]
   [(fresh (rator rand t-x)
      (== `(,rator ,rand) exp)
      (!- rator env `(-> ,t-x ,t))
      (!- rand env t-x))]))

(defrel (lookupo x env t)
  (fresh (rest y v)
    (== `((,y . ,v) . ,rest) env)
    (conde
     ((== y x) (== v t))
     ((=/= y x) (lookupo x rest t)))))

(defrel (not-in-envo x env)
  (conde
   ((== '() env))
   ((fresh (y v rest)
      (== `((,y . ,v) . ,rest) env)
      (=/= y x)
      (not-in-envo x rest)))))

#|
The original test generated the first 10 elements in the list, but
it seems like first-order miniKanren has a slight ordering difference
compared to faster miniKanren, so the ordering has changed.
This version finds the missing element as the 11th element, which is
tested below.
|#

(test "types"
  (run 8 (q) (fresh (t exp) (!- exp '() t)  (== `(,exp => ,t) q)))
  '((((lambda (_.0) _.0) => (-> _.1 _.1)) (sym _.0))
    (((lambda (_.0) (lambda (_.1) _.1))
      =>
      (-> _.2 (-> _.3 _.3)))
     (=/= ((_.0 lambda)))
     (sym _.0 _.1))
    (((lambda (_.0) (lambda (_.1) _.0))
      =>
      (-> _.2 (-> _.3 _.2)))
     (=/= ((_.0 _.1)) ((_.0 lambda)))
     (sym _.0 _.1))
    ((((lambda (_.0) _.0) (lambda (_.1) _.1)) => (-> _.2 _.2))
     (sym _.0 _.1))
    (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.2)))
      =>
      (-> _.3 (-> _.4 (-> _.5 _.5))))
     (=/= ((_.0 lambda)) ((_.1 lambda)))
     (sym _.0 _.1 _.2))
    (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.1)))
      =>
      (-> _.3 (-> _.4 (-> _.5 _.4))))
     (=/= ((_.0 lambda)) ((_.1 _.2)) ((_.1 lambda)))
     (sym _.0 _.1 _.2))
    (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.0)))
      =>
      (-> _.3 (-> _.4 (-> _.5 _.3))))
     (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 lambda)) ((_.1 lambda)))
     (sym _.0 _.1 _.2))
    (((lambda (_.0) (_.0 (lambda (_.1) _.1)))
      =>
      (-> (-> (-> _.2 _.2) _.3) _.3))
     (=/= ((_.0 lambda)))
     (sym _.0 _.1))
    #;(((lambda (_.0) (lambda (_.1) (_.1 _.0)))
      =>
      (-> _.2 (-> (-> _.2 _.3) _.3)))
     (=/= ((_.0 _.1)) ((_.0 lambda)))
     (sym _.0 _.1))
    #;((((lambda (_.0) _.0) (lambda (_.1) (lambda (_.2) _.2)))
        =>
        (-> _.3 (-> _.4 _.4)))
       (=/= ((_.1 lambda)))
       (sym _.0 _.1 _.2))))

(define-test types-subsumed
  (check
   (lset<=
    equal?
    '((((lambda (_.0) _.0) => (-> _.1 _.1)) (sym _.0))
      (((lambda (_.0) (lambda (_.1) _.1))
        =>
        (-> _.2 (-> _.3 _.3)))
       (=/= ((_.0 lambda)))
       (sym _.0 _.1))
      (((lambda (_.0) (lambda (_.1) _.0))
        =>
        (-> _.2 (-> _.3 _.2)))
       (=/= ((_.0 _.1)) ((_.0 lambda)))
       (sym _.0 _.1))
      ((((lambda (_.0) _.0) (lambda (_.1) _.1)) => (-> _.2 _.2))
       (sym _.0 _.1))
      (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.2)))
        =>
        (-> _.3 (-> _.4 (-> _.5 _.5))))
       (=/= ((_.0 lambda)) ((_.1 lambda)))
       (sym _.0 _.1 _.2))
      (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.1)))
        =>
        (-> _.3 (-> _.4 (-> _.5 _.4))))
       (=/= ((_.0 lambda)) ((_.1 _.2)) ((_.1 lambda)))
       (sym _.0 _.1 _.2))
      (((lambda (_.0) (_.0 (lambda (_.1) _.1)))
        =>
        (-> (-> (-> _.2 _.2) _.3) _.3))
       (=/= ((_.0 lambda)))
       (sym _.0 _.1))
      (((lambda (_.0) (lambda (_.1) (lambda (_.2) _.0)))
        =>
        (-> _.3 (-> _.4 (-> _.5 _.3))))
       (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 lambda)) ((_.1 lambda)))
       (sym _.0 _.1 _.2))
      (((lambda (_.0) (lambda (_.1) (_.1 _.0)))
        =>
        (-> _.2 (-> (-> _.2 _.3) _.3)))
       (=/= ((_.0 _.1)) ((_.0 lambda)))
       (sym _.0 _.1))
      ((((lambda (_.0) _.0) (lambda (_.1) (lambda (_.2) _.2)))
        =>
        (-> _.3 (-> _.4 _.4)))
       (=/= ((_.1 lambda)))
       (sym _.0 _.1 _.2)))
    (run 13 (q) (fresh (t exp) (!- exp '() t)  (== `(,exp => ,t) q))))))
