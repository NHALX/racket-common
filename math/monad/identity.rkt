#lang racket
(require "../monad.rkt")

(provide monad/id id:run)
(module+ test (require rackunit))
         
#| ﻿Ｉｄｅｎｔｉｔｙ Ｍｏｎａｄ
   note: captures multiple args as lists |#

(define (id:>>= xs f)     (apply f xs))
(define (id:return . xs)  xs)


(define-syntax-rule (id:run f)
  (parameterize [[current:>>=    id:>>=]
                 [current:return id:return]]
    f))


(define-syntax-rule (monad/id : xs ...)
  (id:run (do: xs ...)))


(module+ test
  
  (check-equal?
   5
   (id:>>= (id:return 1 2) (λ (x y) (+ y x 2))))
  
  (check-equal?
   5
   (monad/id :
             x y ← (return 1 2)
             (+ y x 2)))

  (law:left-identity  monad/id)
  (law:right-identity monad/id)
  (law:associativity  monad/id))
