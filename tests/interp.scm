#!r6rs

(import (rnrs)
        (chrKanren interp) (chrKanren streams) (chrKanren goals)
        (chrKanren relation) (chrKanren vars) (chrKanren test)
        (chrKanren state) (chrKanren subst)
        (srfi :64 testing)
        (srfi :39 parameters)
        (srfi :27 random-bits))

(define (test-mature-stream name strm)
  (test-assert (string-append name "-mature?") (mature? strm))
  (test-equal  (string-append name "-age") strm (age strm))
  (test-equal  (string-append name "-mature") strm (mature strm)))

(test-begin "interp")

(test-runner-install-random-hooks! (test-runner-current))

(test-mature-stream "empty-stream" empty-stream)
(test-mature-stream "solution-stream" (make-solution 1 (make-bind empty-stream fail)))

(test-assert "bind is immature" (not (mature? (make-bind empty-stream succeed))))

(define-random-test (bind-left-identity [g (random-goal)])
  (test-assert "(make-bind empty-stream g) ≈ empty-stream"
    (empty? (mature-finite (make-bind empty-stream g)))))

#;
(define-random-test (bind-right-zero [r (random-stream)])
  (test-assert "(make-bind r fail) ≈ empty-stream"
    (empty? (mature-finite (make-bind r fail)))))

#;
(define-random-test (bind-right-identity [r (random-stream)])
  (test-equal "(make-bind r succeed) ≈ r"
    (take-finite r)
    (take-finite (make-bind r succeed))))

;; (define-random-test "(make-choice r empty-stream) ≈ (make-choice empty-stream r) ≈ r"
;;   (let* ((r (random-stream)))
;;     (test-equal "left-identity"
;;       (take r)
;;       (take (make-choice r empty-stream)))
;;     (test-equal "right-identity"
;;       (take r)
;;       (take (make-choice empty-stream r)))))

;; (define-random-test "(disj k j) ≈ (disj j k)"
;;   (let* ((j (random-goal))
;;          (k (random-goal))
;;          (s (random-state)))
;;     (test-equal (take (start s (disj j k)))
;;       (take (start s (disj k j))))))

(test-end "interp")
