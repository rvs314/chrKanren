#!r6rs

(library (chrKanren unify)
  (export walk walk* unify unify* square-varmap subterm?)
  (import (rnrs)
          (except (srfi :1 lists) for-each assoc map fold-right member find filter partition remove car+cdr)
          (chrKanren utils)
          (chrKanren check)
          (chrKanren varmap)
          (chrKanren vars))

  (define walk*
    (case-lambda
      [(obj vm)
       (walk* obj vm var?)]
      [(obj vm var?)
       (cond
         [(pair? obj) (cons (walk* (car obj) vm var?) (walk* (cdr obj) vm var?))]
         [(var? obj)
          (let ([rs (walk obj vm var?)])
            (if (var? rs)
                rs
                (walk* rs vm var?)))]
         [else obj])]))

  (define walk
    (case-lambda
      [(obj vm)
       (walk obj vm var?)]
      [(obj vm var?)
       (let walk ([obj obj])
         (if (var? obj)
             (let ([v0 (varmap-lookup obj vm)])
                 (if (eq? v0 obj)
                     obj
                     (walk v0)))
             obj))]))

  (define (square-varmap vm)
    (check (varmap? vm))
    (alist->varmap (map (lambda (k) (cons k (walk* k vm)))
                        (delete-duplicates (map car (varmap->alist vm))))))

  (define (subterm? head tail vm)
    (or (eq? head tail)
        (and (pair? tail)
             (or (subterm? head (car tail) vm)
                 (subterm? head (cdr tail) vm)))
        (let ([v0 (walk tail vm)])
          (and (not (eq? tail v0))
               (subterm? head v0 vm)))))

  (define (extend var val vm)
    (and (not (subterm? var val vm))
         (varmap-extend var val vm)))

  (define unify*
    (case-lambda
      [(objs vm)
       (unify* objs vm var?)]
      [(objs vm var?)
       (unify (map car objs) (map cdr objs) vm var?)]))

  (define unify
    (case-lambda
      [(lhs rhs vm)
       (unify lhs rhs vm var?)]
      [(lhs* rhs* vm var?)
       (define lhs (walk lhs* vm))
       (define rhs (walk rhs* vm))
       (cond
         [(eq? lhs rhs) vm]
         [(and (var? lhs) (var? rhs))
          (if (> (var-idx lhs) (var-idx rhs))
              (extend lhs rhs vm)
              (extend rhs lhs vm))]
         [(var? lhs) (extend lhs rhs vm)]
         [(var? rhs) (extend rhs lhs vm)]
         [(and (pair? lhs) (pair? rhs))
          (let ([vm0 (unify (car lhs) (car rhs) vm var?)])
            (and vm0 (unify (cdr lhs) (cdr rhs) vm0 var?)))]
         [(equal? lhs rhs) vm]
         [else #f])])))
