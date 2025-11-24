#!r6rs

(import (rnrs)
        (except (chrKanren base) run run*)
        (only (srfi :1 lists) lset<=)
        (chrKanren test)
        (chrKanren compare)
        (chrKanren tests fmk shim))

(defrel (evalo expr val)
  (eval-expro expr '() val))

(defrel (eval-expro expr env val)
  (conde
   ((fresh (rator rand x body env^ a)
      (== `(,rator ,rand) expr)
      (eval-expro rator env `(closure ,x ,body ,env^))
      (eval-expro rand env a)
      (eval-expro body `((,x . ,a) . ,env^) val)))
   ((fresh (x body)
      (== `(lambda (,x) ,body) expr)
      (symbolo x)
      (== `(closure ,x ,body ,env) val)
      (not-in-envo 'lambda env)))
   ((symbolo expr) (lookupo expr env val))))

(defrel (not-in-envo x env)
  (conde
   ((== '() env))
   ((fresh (y v rest)
      (== `((,y . ,v) . ,rest) env)
      (=/= y x)
      (not-in-envo x rest)))))

(defrel (lookupo x env t)
  (conde
   ((fresh (y v rest)
      (== `((,y . ,v) . ,rest) env) (== y x)
      (== v t)))
   ((fresh (y v rest)
      (== `((,y . ,v) . ,rest) env) (=/= y x)
      (lookupo x rest t)))))

(define-test running-backwards
  (check
   (equal?
    (lex-sort (run 5 (q) (evalo q '(closure y x ((x . (closure z z ())))))))
    (lex-sort '(((lambda (x) (lambda (y) x)) (lambda (z) z))
                ((lambda (x) (x (lambda (y) x))) (lambda (z) z))
                (((lambda (x) (lambda (y) x))
                  ((lambda (_.0) _.0) (lambda (z) z)))
                 (sym _.0))
                (((lambda (_.0) _.0)
                  ((lambda (x) (lambda (y) x)) (lambda (z) z)))
                 (sym _.0))
                ((((lambda (_.0) _.0) (lambda (x) (lambda (y) x)))
                  (lambda (z) z))
                 (sym _.0)))))))

(test "eval-exp-lc 1"
  (run* (q) (evalo '(((lambda (x) (lambda (y) x)) (lambda (z) z)) (lambda (a) a)) q))
  '((closure z z ())))

(test "eval-exp-lc 2"
  (run* (q) (evalo '((lambda (x) (lambda (y) x)) (lambda (z) z)) q))
  '((closure y x ((x . (closure z z ()))))))

(define-test fully-running-backwards
  (check (lset<= equal?
                 '((((lambda (_.0) _.1) ==> (closure _.0 _.1 ()))
                    (sym _.0))
                   ((((lambda (_.0) _.0) (lambda (_.1) _.2)) ==> (closure _.1 _.2 ())) (sym _.0 _.1))
                   ((((lambda (_.0) (lambda (_.1) _.2)) (lambda (_.3) _.4))
                     ==> (closure _.1 _.2 ((_.0 . (closure _.3 _.4 ())))))
                    (=/= ((_.0 lambda)))
                    (sym _.0 _.1 _.3))
                   ((((lambda (_.0) (_.0 _.0)) (lambda (_.1) _.1)) ==> (closure _.1 _.1 ()))
                    (sym _.0 _.1))
                   ((((lambda (_.0) (_.0 _.0))
                      (lambda (_.1) (lambda (_.2) _.3)))
                     ==>
                     (closure _.2 _.3 ((_.1 . (closure _.1 (lambda (_.2) _.3) ())))))
                    (=/= ((_.1 lambda)))
                    (sym _.0 _.1 _.2)))
                  (run 10 (q)
                    (fresh (e v)
                      (evalo e v)
                      (== `(,e ==> ,v) q))))))
