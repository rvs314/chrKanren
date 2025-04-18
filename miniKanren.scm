#!r6rs

(library (chrKanren miniKanren)
  (export fresh conde run run* take take-all take+drop reify-1st)
  (import (rnrs) (chrKanren microKanren))

  (define-syntax Zzz
      (syntax-rules ()
        ((_ g) (lambda (s/c) (lambda () (g s/c))))))

  (define-syntax conj+
    (syntax-rules ()
      ((_ g) (Zzz g))
      ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

  (define-syntax disj+
    (syntax-rules ()
      ((_ g) (Zzz g))
      ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

  (define-syntax fresh
    (syntax-rules ()
      ((_ () g0 g ...) (conj+ g0 g ...))
      ((_ (x0 x ...) g0 g ...)
       (call/fresh
        (lambda (x0)
          (fresh (x ...) g0 g ...))))))

  (define-syntax conde
    (syntax-rules ()
      ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

  (define-syntax run
    (syntax-rules ()
      ((_ n (x ...) g0 g ...)
       (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))))

  (define-syntax run*
    (syntax-rules ()
      ((_ (x ...) g0 g ...)
       (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))))

  (define (call/goal g) (g empty-state))

  (define (pull $)
    (define seen-thunks (make-eq-hashtable))
    (let loop (($ $))
      (if (procedure? $)
          (begin
            (when (hashtable-contains? seen-thunks $)
              (error 'pull "Attempting to pull childish stream"))
            (hashtable-set! seen-thunks $ #t)
            (loop ($)))
          $)))

  (define (take-all $)
    (let (($ (pull $)))
      (if (null? $)
          '()
          (cons (car $) (take-all (cdr $))))))

  (define (take+drop n $)
    (if (zero? n)
        (values '() $)
        (let (($ (pull $)))
          (if (null? $)
              (values '() '())
              (let-values (((h t) (take+drop (- n 1) (cdr $))))
                (values (cons (car $) h) t))))))

  (define (take n $)
    (let-values (((h t) (take+drop n $)))
      h))

  (define (reify-1st state)
    (let* ((v  (walk* (var 0) (state-assoc state)))
           (s  (reify-s (state-constraints state) (reify-s v '())))
           (v^ (walk* v s)))
      (if (null? (state-constraints state))
          v^
          (list v^ 'WHERE (walk* (state-constraints state) s)))))

  (define (reify-s v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (let ((n (reify-name (length s))))
           (cons `(,v . ,n) s)))
        ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
        (else s))))

  (define (reify-name n)
    (string->symbol
      (string-append "_" "." (number->string n))))

  (define (fresh/nf n f)
    (letrec
      ((app-f/v*
         (lambda (n v*)
           (cond
             ((zero? n) (apply f (reverse v*)))
             (else (call/fresh
                     (lambda (x)
                       (app-f/v* (- n 1) (cons x v*)))))))))
      (app-f/v* n '()))))
