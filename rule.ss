#!r6rs

(library (chrKanren rule)
  (export define-rules
          *constraint-handling-rules*
          forall
          witness witness? make-witness kept-witness removed-witness
          witness-kept? witness-removed? witness-constraint
          rule rule? rule-id rule-prereqs rule-consequences
          rule-free-variables rule-free?
          parse-rule keep forget
          scheme ground)
  (import (rnrs)
          (srfi :39 parameters)
          (chrKanren check)
          (chrKanren utils)
          (chrKanren vars)
          (chrKanren goals))

  (define *rule-counter* 0)

  (define-record-type rule
    (fields id free-variables prereqs consequences)
    (protocol
     (lambda (new)
       (define (make-rule free-variables prereqs consequences)
         (let ([id *rule-counter*])
           (set! *rule-counter* (+ *rule-counter* 1))
           (new id free-variables prereqs consequences)))
       make-rule)))

  (define-check (rule-free? [rule rule?] var)
    any?
    (and (var? var)
         (memq var (rule-free-variables rule))))

  (define *constraint-handling-rules* (make-parameter '()))

  (define-syntax forall (syntax-rules ()))
  (define-syntax keep (syntax-rules ()))
  (define-syntax forget (syntax-rules ()))

  (define-record-type witness
    (fields kept? constraint)
    (protocol
     (lambda (new)
       (define-check (make-witness [kept? boolean?]
                                   [posting posting?])
         witness?
         (new kept? posting))
       make-witness)))

  (define witness-removed? (negate witness-kept?))

  (define (kept-witness constraint)
    (make-witness #t constraint))
  (define (removed-witness constraint)
    (make-witness #f constraint))

  (define-syntax parse-rule
    (syntax-rules (keep forget forall =>)
      [(parse-rule (vs ...) (gl ...) ())
       (parse-rule (vs ...) (gl ...) (=> succeed))]
      [(parse-rule (vs ...) (gl ...) (=> res ...))
       (fresh (vs ...)
         (make-rule
          (list vs ...)
          (list gl ...)
          (lambda (vs ...) (conj res ...))))]
      [(parse-rule (vs ...) (gl ...) ((keep g₀) rest ...))
       (parse-rule (vs ...) (gl ... (kept-witness g₀)) (rest ...))]
      [(parse-rule (vs ...) (gl ...) ((forget g₀) rest ...))
       (parse-rule (vs ...) (gl ... (removed-witness g₀)) (rest ...))]
      [(parse-rule (vs ...) (gl ...) ((keep g₀ ...) rest ...))
       (parse-rule (vs ...) (gl ...) ((keep g₀) ... rest ...))]
      [(parse-rule (vs ...) (gl ...) ((forget g₀ ...) rest ...))
       (parse-rule (vs ...) (gl ...) ((forget g₀) ... rest ...))]
      [(parse-rule (vs ...) (gl ...) (g₀ rest ...))
       (parse-rule (vs ...) (gl ...) ((keep g₀) rest ...))]
      [(parse-rule (forall (vs ...) rest ...))
       (parse-rule (vs ...) () (rest ...))]))

  (define-syntax-rule (define-rules rule ...)
    (define rules
      (*constraint-handling-rules*
       (append (*constraint-handling-rules*)
               (list (parse-rule rule) ...)))))

  (define (ground pred . os)
    (define (only-ground? . os) (for-all (negate var?) os))
    (apply scheme (conjoin only-ground? pred) os)))
