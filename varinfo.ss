#!r6rs

(library (chrKanren varinfo)
  (export exact exactly exact?
          partial at-least partial?
          no-info)
  (import (except (rnrs) exact exact?) (chrKanren utils) (chrKanren check))

  (define-record-type (exact exactly exact?)
    (fields value))

  (define-record-type (partial at-least partial?)
    (fields constraints)
    (protocol
     (lambda (new)
       (lambda (obj)
         (check (list? obj))
         (new obj)))))

  (define no-info (at-least '())))
