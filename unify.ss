#!r6rs

(library (chrKanren unify)
  (export unify)
  (import (rnrs) (chrKanren subst) (chrKanren state) (chrKanren streams)
          (chrKanren vars) (chrKanren goals))

  (define (unify lhs* rhs* st)
    (define lhs (walk lhs* (state-subst st)))
    (define rhs (walk rhs* (state-subst st)))
    (cond
      [(eq? lhs rhs)  (make-singleton st)]
      [(or (var? lhs) (var? rhs))
       (let*-values ([(yng old) (younger+older-var lhs rhs)]
                     [(ex) (state-extend yng old st)])
         (if ex
             (make-singleton ex)
             empty-stream))]
      [(and (pair? lhs) (pair? rhs))
       (make-bind (unify (car lhs) (car rhs) st)
                  (== (cdr lhs) (cdr rhs)))]
      [(equal? lhs rhs) (make-singleton st)]
      [else empty-stream])))
