#!r6rs

(library (chrKanren tests fmk shim)
  (export test defrel)
  (import (rnrs) (rnrs eval) (chrKanren utils) (chrKanren base) (chrKanren check) (chrKanren test))

  (define-syntax-rule (test nm operation result)
    (define-test check-test
      (check (equal? operation result) nm)))

  (define-syntax-rule (defrel arg ...)
    (define-relation arg ...)))

  ;; (define-syntax run*-shim
  ;;   (syntax-rules ()
  ;;     [(run*-shim (r v vs ...) q ...)
  ;;      (run* (r v vs ...) q ...)]
  ;;     [(run*-shim (v) q ...)
  ;;      (map car (run* (v) q ...))]))

  ;; (define-syntax run-shim
  ;;   (syntax-rules ()
  ;;     [(run-shim k (r v vs ...) q ...)
  ;;      (run k (r v vs ...) q ...)]
  ;;     [(run-shim k (v) q ...)
  ;;      (map car (run k (v) q ...))])))
