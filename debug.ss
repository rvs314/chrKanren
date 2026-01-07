#!r6rs

(library (chrKanren debug)
  (export *debugging?* debug)
  (import (rnrs)
          (chrKanren utils)
          (srfi :39 parameters)
          (only (srfi :1 lists) last))

  (define *debugging?* (make-parameter #f))

  (define (debug . msg)
    (when (*debugging?*)
      (apply puts msg))
    (last msg)))
