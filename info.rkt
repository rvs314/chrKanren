#lang info

(define collection 'use-pkg-name)
(define license 'MIT)
(define version "0.0")
(define deps '("base"))

;; Ignore non-test files
(define test-omit-paths (list #rx"/chrKanren/(?!tests)"))
