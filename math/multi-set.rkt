#lang racket/base
(require "../NHA.rkt")
(provide multi-set
         list->multi-set
         multi-set->list)



(define (list->multi-set xs)
  (make-immutable-hash   
   (map (λ (xs)
          (cons (car xs)
                (length xs)))
        
        (equiv-classes equal? xs))))


(define (multi-set->list set)
  (apply append
         (hash-map set (λ (k v)
                         (build-list v (λ (x) k))))))

(define (multi-set . xs)
  (make-immutable-hash xs))




(module+ test
  (require rackunit)
  
  (check-equal?
   (multi-set->list (list->multi-set '(0 1 1 1 2 3)))
   '(0 1 1 1 2 3)
   "multi-set->list"))


