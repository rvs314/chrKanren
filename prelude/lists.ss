#!r6rs

(library (chrKanren prelude lists)
  (export nullo pairo conso caro cdro
          fold-lefto fold-lefto*
          mapo map*o
          appendo append*o
          assoco lookupo)
  (import (rnrs)
          (chrKanren utils)
          (chrKanren base)
          (chrKanren prelude applyo))

  (define-relation (nullo obj)
    (== obj '()))

  (define-relation (pairo obj)
    (fresh (a d)
      (== obj (cons a d))))

  (define-relation (conso l r l.r)
    (== (cons l r) l.r))

  (define-relation (caro l.r l)
    (fresh (r)
      (conso l r l.r)))

  (define-relation (cdro l.r r)
    (fresh (l)
      (conso l r l.r)))

  (define-relation (map-conso ls rs l.rs)
    (conde
     [(== ls '()) (== rs '()) (== l.rs '())]
     [(fresh (l lss r rss l.rss)
        (== ls   (cons l lss))
        (== rs   (cons r rss))
        (== l.rs (cons (cons l r) l.rss))
        (map-conso lss rss l.rss))]))

  (define-relation (map-nullo ls)
    (conde
     [(== ls '())]
     [(fresh (r)
        (== ls (cons '() r))
        (map-nullo r))]))

  (define-relation (append2o xs ys zs)
    (conde
     [(== xs '()) (== ys zs)]
     [(fresh (x xss zss)
        (== xs (cons x xss))
        (== zs (cons x zss))
        (append2o xss ys zss))]))

  (define (fold-lefto patho start edge1 . edges+finish)
    (let-values ([(finish edges*)
                  (ref-and-remove (length edges+finish)
                                  (cons edge1 edges+finish))])
      (fold-lefto* patho start edges* finish)))

  (define-relation (fold-lefto* patho start edges* finish)
    (conde
     [(map-nullo edges*) (== start finish)]
     [(fresh (e1s ess arglist step)
        (map-conso e1s ess edges*)
        (append2o (cons start e1s) (list step) arglist)
        (applyo patho arglist)
        (fold-lefto* patho step ess finish))]))

  (define-relation (map*o rel arg*)
    (conde
     [(map-nullo arg*)]
     [(fresh (a1s gss)
        (map-conso a1s gss arg*)
        (applyo rel a1s)
        (map*o rel gss))]))

  (define (mapo rel . args*)
    (map*o rel args*))

  (define-relation (append*o xs* rs)
    (fold-lefto append2o '() xs* rs))

  (define (appendo lst . lsts+res)
    (let-values ([(res lsts) (ref-and-remove (length lsts+res)
                                             (cons lst lsts+res))])
      (append*o lsts res)))

  (define-relation (assoco key alist res)
    (conde
     [(== alist '()) (== res #f)]
     [(fresh (a d rst)
        (== alist (cons (cons a d) rst))
        (conde
         [(==  key a) (== res (cons a d))]
         [(=/= key a) (assoco key rst res)]))]))

  (define-relation (lookupo key alist res)
    (fresh (pr)
      (assoco key alist pr)
      (cdro pr res))))
