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
         [(vector? obj) (vector-map (lambda (obj) (walk* obj vm var?)) obj)]
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
    (alist->varmap
     (filter (lambda (k.v) (not (equal? (car k.v) (cdr k.v))))
             (map (lambda (k) (cons k (walk* k vm)))
                  (delete-duplicates (map car (varmap->alist vm)))))))

  (define (subterm? needle haystack vm)
    (or (eq? needle haystack)
        (and (pair? haystack)
             (or (subterm? needle (car haystack) vm)
                 (subterm? needle (cdr haystack) vm)))
        (and (vector? haystack)
             (vector-exists
              (lambda (el) (subterm? needle el vm))
              haystack))
        (and (var? haystack)
             (let ([v0 (walk haystack vm)])
               (and (not (eq? haystack v0))
                    (subterm? needle v0 vm))))))

  ;; This version of subterm is particularly anxious,
  ;; and therefore keeps a table of what it's seen so far.
  ;; If `haystack` is proven to be cyclic, then it raises an error.
  #;(define (subterm? needle haystack vm)
      (define seen-table (make-equal-hashtable))
      (define (subterm? needle haystack vm)
        (if (hashtable-ref seen-table haystack #f)
            (error 'subterm? "Detected a cyclic term" haystack)
            (hashtable-set! seen-table haystack #t))
        (or (eq? needle haystack)
            (and (pair? haystack)
                 (or (subterm? needle (car haystack) vm)
                     (subterm? needle (cdr haystack) vm)))
            (and (vector? haystack
                   (vector-exists
                    (lambda (el) (subterm? needle el vm))
                    haystack))
              (let ([v0 (walk haystack vm)])
                (and (not (eq? haystack v0))
                     (subterm? needle v0 vm))))))
      (subterm? needle haystack vm))

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
         [(and (vector? lhs) (vector? rhs))
          (and (eqv? (vector-length lhs) (vector-length rhs))
               (vector-fold
                (lambda (acc v1 v2)
                  (and acc (unify v1 v2 acc var?)))
                vm
                lhs
                rhs))]
         [(and (pair? lhs) (pair? rhs))
          (let ([vm0 (unify (car lhs) (car rhs) vm var?)])
            (and vm0 (unify (cdr lhs) (cdr rhs) vm0 var?)))]
         [(equal? lhs rhs) vm]
         [else #f])])))
