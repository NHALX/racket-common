#lang racket
(require "../monad.rkt")
(provide monad/cps cps:run cps:callCC)

#| Ｃｏｎｔｉｎｕａｔｉｏｎ  Ｍｏｎａｄ |#

(define (cps:return a)
  (λ (next) (next a)))
  
(define (cps:>>= c f)
  (λ (next)
    (c (λ xs
         ((apply f xs) next)))))

(define (cps:callCC f)
  (λ (outer-continuation)
    (let [[backtrack (λ (result)
                       (λ (ignored-continuation)
                         (outer-continuation result)))]]
      
     ((f backtrack) outer-continuation))))


(define-syntax-rule (cps:run f next)
  (parameterize [[current:>>=    cps:>>=]
                 [current:return cps:return]]
    (f next)))


(define-syntax-rule (monad/cps : f xs ...)
  (cps:run (do: f xs ...) identity))



(module+ test
  (require rackunit)
  
  (check-equal?
   (monad/cps :
              x <- (return 1)
              y <- (return -1)
              z <- (cps:callCC
                    (λ (k)
                      (do:
                          (k 9)
                        (return "unreachable"))))  
              
              w <- (return 1)
              (return (+ w x y z)))

   10))
 
  
(module+ test
  
  (define (g last1 last2 next)
    (next (+ last1 last2 2)))
  
  (define (f:one-and-two next)
    (next 1 2))
  
  (define (g:x+y+2 last1 last2 next)
    (next (+ last1 last2 2)))
  
  (check-equal?
   2
   ((cps:>>= (cps:return 1)
             (λ (one) (cps:return (+ 1 one))))
    identity))
  
  (check-equal?
   3
   (monad/cps :
              x ← (return 1)
              (return (+ 2 x))))
  
  (define (test x next #:keyword [kw #f])
    (next (string-join x #:after-last kw)))
  
  (check-equal?
   "a b c"
   (monad/cps :
              x <- (return (list "a" "b"))
              (test x #:keyword " c")))
  
  (check-equal?
   8
   (monad/cps :
              one two   <- (f:one-and-two)
              five      <- (g:x+y+2 one two)
              (return (identity one))
              nine      ← (return 9)
              one_also  ← (return (identity one))
              
              (return 7)
              (g:x+y+2 five one_also)))
  
  (law:left-identity  monad/cps)
  (law:right-identity monad/cps)
  (law:associativity  monad/cps))
