#!r6rs

(library (chrKanren vars)
  (export *var-counter*
          var var-name make-var var?
          free-variables
          fresh var<=?
          varmap? empty-varmap
          varmap-lookup varmap-extend
          alist->varmap varmap->alist
          varmap-copy)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren goals)
          (chrKanren check)
          (chrKanren hashmap)
          (srfi :39 parameters))

  (define-check (assert-natural [x natural?])
    natural?
    x)

  (define *var-counter* (make-parameter 0 assert-natural))

  (define-syntax fresh
    (syntax-rules ()
      [(fresh (var-name ...) body)
       (let* ([var-name (make-var 'var-name)]
              ...)
         body)]
      [(fresh (var-name ...) body body* ...)
       (fresh (var-name ...) (conj body body* ...))]))

  (define-record-type var
    (fields name idx)
    (protocol
     (lambda (new)
       (lambda (name)
         (*var-counter* (+ 1 (*var-counter*)))
         (new name (- (*var-counter*) 1))))))

  ;; TODO: rename this to `variables`
  (define (free-variables obj)
    (cond
      [(var? obj) (list obj)]
      [(pair? obj) (append (free-variables (car obj))
                           (free-variables (cdr obj)))]
      [(vector? obj) (free-variables (vector->list obj))]
      [else '()]))

  (define var<=? (on <= var-idx))

  (define-hashmap-type varmap var? any? var-idx)

 (define-check (varmap-lookup [key var?] [vmap varmap?])
    any?
    (hashmap-ref vmap key (lambda () key)))

 (define alist? (listof (pairof var? any?)))

  (define-check (varmap-extend [key var?] [value any?] [vmap varmap?])
    varmap?
    (hashmap-set vmap key value)))
