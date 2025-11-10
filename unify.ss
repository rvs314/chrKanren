#!r6rs

(library (chrKanren unify)
  (export walk walk* unify)
  (import (rnrs)
          (chrKanren utils)
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
       (fixpoint
        (lambda (obj)
          (if (var? obj)
              (varmap-lookup obj vm)
              obj))
        eq?
        obj)]))

  (define unify
    (case-lambda
      [(lhs rhs vm)
       (unify lhs rhs vm var?)]
      [(lhs* rhs* vm var?)
       (define lhs (walk lhs* vm))
       (define rhs (walk rhs* vm))
       (cond
         [(eq? lhs rhs) vm]
         [(or (var? lhs) (var? rhs))
          (let*-values ([(target value)
                         (if (var? lhs)
                             (values lhs rhs)
                             (values rhs lhs))]
                        [(ex) (varmap-extend target value vm)])
            ex)]
         [(and (pair? lhs) (pair? rhs))
          (let ([vm0 (unify (car lhs) (car rhs) vm var?)])
            (and vm0 (unify (cdr lhs) (cdr rhs) vm0 var?)))]
         [(equal? lhs rhs) vm]
         [else #f])])))
