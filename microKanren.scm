(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define-record-type state
  (fields assoc constraints count))

(define (state-with-assoc state assoc)
  (make-state assoc (state-constraints state) (state-count state)))

(define (allocate-variable state)
  (values (var (state-count state))
          (make-state (state-assoc state) (state-constraints state) (+ 1 (state-count state)))))

(define (add-constraint state new-constraint)
  (make-state (state-assoc state)
              (cons new-constraint (state-constraints state))
              (state-count state)))

(define (=/= left right)
  (lambda (st)
    (unit (add-constraint st `(=/= ,left ,right)))))

(define empty-state (make-state '() '() 0))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (== u v)
  (lambda (st)
    (let ((s (unify u v (state-assoc st))))
      (if s
          (unit (state-with-assoc st s))
          mzero))))

(define (unit state) (cons state mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (st)
    (let-values (((vr st^) (allocate-variable st)))
      ((f vr) st^))))

(define (disj g1 g2) (lambda (state) (mplus (g1 state) (g2 state))))
(define (conj g1 g2) (lambda (state) (bind (g1 state) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))
