#!r6rs

(library (chrKanren subst)
  (export empty-subst extend lookup walk walk*)
  (import (rnrs) (chrKanren vars))

  (define empty-subst '())

  (define (extend key value subst)
    (assert (var? key))
    (and (not (occurs-in? key value))
         (cons (cons key value) subst)))

  (define (lookup obj subst)
    (cond [(null? subst)            (error 'lookup "Cannot find variable" obj)]
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
          [(pair? haystack) (or (occurs-in? needle (walk (car haystack) subst))
                                (occurs-in? needle (walk (cdr haystack) subst)))]
          [else             #f])))
