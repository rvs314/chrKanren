#!r6rs

(library (chrKanren subst)
  (export empty-subst extend lookup walk walk* subst->goal)
  (import (rnrs) (chrKanren vars) (chrKanren streams) (chrKanren goals))

  (define empty-subst '())

  (define (extend key value subst)
    (assert (var? key))
    (and (not (occurs-in? key value subst))
         (cons (cons key value) subst)))

  (define (lookup obj subst)
    ;; TODO: This should just be a call to assp
    (cond [(null? subst)            obj]
          [(var=? (caar subst) obj) (cdar subst)]
          [(pair? subst)            (lookup obj (cdr subst))]
          [else                     obj]))

  (define (walk term subst)
    (if (var? term)
        (let ((res (lookup term subst)))
          (if (var=? term res)
              term
              (walk res subst)))
        term))

  (define (walk* term subst)
    (cond
      [(pair? term) (cons (walk* (car term) subst)
                          (walk* (cdr term) subst))]
      [(var? term)  (let ([res (walk term subst)])
                      (if (var=? res term)
                          term
                          (walk* res subst)))]
      [else         term]))

  (define (occurs-in? needle haystack subst)
    (cond [(var? haystack)  (var=? needle haystack)]
          [(pair? haystack) (or (occurs-in? needle (walk (car haystack) subst) subst)
                                (occurs-in? needle (walk (cdr haystack) subst) subst))]
          [else             #f]))

  (define (subst->goal subst)
    (conj (map (lambda (k.v) (== (car k.v) (cdr k.v))) subst))))
