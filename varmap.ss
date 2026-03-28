#!r6rs

(library (chrKanren varmap)
  (export empty-varmap
          varmap
          varmap?
          varmap-lookup
          varmap-extend
          varmap-extend-all
          varmap->alist alist->varmap
          varmap-copy)
  (import (rnrs)
          (chrKanren check)
          (chrKanren intmaps)
          (chrKanren vars)
          (chrKanren utils))

  (define-record-type varmap (fields contents))

  (define empty-varmap (make-varmap '()))

  (define-check (varmap-lookup [key var?] [vmap varmap?])
    any?
    (define res (intmap-ref (varmap-contents vmap) (var-idx key)))
    (if (not-found? res)
        key
        (cdr res)))

  (define-check (varmap-extend [key var?] [value any?] [vmap varmap?])
    varmap?
    (define imap  (varmap-contents vmap))
    (define imap^ (intmap-set imap (var-idx key) (cons key value)))
    (define vmap^ (make-varmap imap^))
    vmap^)

  (define alist? (listof (pairof var? any?)))

  (define-check (varmap-extend-all [alist alist?]
                                   [vmap varmap?])
    varmap?
    (fold-left (lambda (acc k.v) (varmap-extend (car k.v) (cdr k.v) acc)) vmap alist))

  (define-check (varmap->alist [vm varmap?]) alist?
    (intmap-values (varmap-contents vm)))

  (define-check (alist->varmap [alist alist?]) varmap?
    (varmap-extend-all alist empty-varmap))

  (define-check (varmap-copy [vm varmap?]) varmap?
    (alist->varmap
     (map (lambda (k.v)
            (cons (car k.v) (copy-object (cdr k.v))))
          (varmap->alist vm)))))
