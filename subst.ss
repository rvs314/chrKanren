#!r6rs

(library (chrKanren subst)
  (export empty-subst extend lookup walk walk* subst->goal
          subst=?
          reify-var-subst)
  (import (rnrs)
          (only (srfi :1 lists) lset-union)
          (chrKanren vars) (chrKanren streams) (chrKanren goals))

  (define empty-subst '())

  (define (extend key value subst)
    (assert (var? key))
    (and (not (occurs-in? key value subst))
         (cons (cons key value) subst)))

  (define (lookup obj subst)
    ;; TODO: This should just be a call to assp
    (cond [(null? subst)            obj]
          [(eq? (caar subst) obj) (cdar subst)]
          [(pair? subst)            (lookup obj (cdr subst))]
          [else                     obj]))

  (define (walk term subst)
    (if (var? term)
        (let ((res (lookup term subst)))
          (if (eq? term res)
              term
              (walk res subst)))
        term))

  (define (walk* term subst)
    (cond
      [(pair? term) (cons (walk* (car term) subst)
                          (walk* (cdr term) subst))]
      [(var? term)  (let ([res (walk term subst)])
                      (if (eq? res term)
                          term
                          (walk* res subst)))]
      [else         term]))

  (define (occurs-in? needle haystack subst)
    (cond [(var? haystack)
           (eq? needle haystack)]
          [(pair? haystack)
           (or (occurs-in? needle (walk (car haystack) subst) subst)
               (occurs-in? needle (walk (cdr haystack) subst) subst))]
          [else             #f]))

  (define (assoc->goal assoc)
    (== (car assoc) (cdr assoc)))

  (define (subst->goal subst)
    (apply conj (map assoc->goal subst)))

  (define (subst-domain subst)
    (map car subst))

  (define (subst=? left right)
    (define domain
      (lset-union eq?
                  (subst-domain left)
                  (subst-domain right)))
    (for-all (lambda (var)
               (equal? (lookup var left)
                       (lookup var right)))
             domain))

  (define (reify-var-subst var subst)
    (walk* var subst)))
