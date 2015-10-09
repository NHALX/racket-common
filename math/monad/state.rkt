#lang racket/base
(require "../monad.rkt" racket/match)
(provide ST:get ST:put ST:update ST:run monad/ST)

#|﻿ Ｓｔａｔｅ Ｍｏｎａｄ |#

(define (ST:return a)
  (λ (st) (cons a st)))
  
(define (ST:>>= c f)
  (λ (st)
    (match-let [[(cons x st2) (ST:run c st)]]
      (ST:run (f x) st2))))


(define-syntax-rule (ST:run f next)
  (parameterize [[current:>>=    ST:>>=]
                 [current:return ST:return]]
    (f next)))


(define-syntax-rule (monad/ST init : f xs ...)
  (ST:run (do: f xs ...) init))

(define (ST:get)
  (λ (st) (cons st st)))

(define (ST:put x)
  (λ (st) (cons st x)))

(define (ST:update f)
  (λ (st) (cons st (f st))))

(module+ test

  (law:left-identity  monad/ST 0)
  (law:right-identity monad/ST 0)
  (law:associativity  monad/ST 0))
