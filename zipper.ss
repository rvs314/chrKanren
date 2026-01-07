#!r6rs

(library (chrKanren zipper)
  (export)
  (import (rnrs) (chrKanren utils))

  (define-record-type zipper
    (fields left center right)))
