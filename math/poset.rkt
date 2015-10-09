#lang racket/base
(provide (all-defined-out))
(require racket/match)
(require racket/list)
(require racket/set)
(require "../NHA.rkt")
(require "multi-set.rkt")

(module+ test 

  (require rackunit)
  (require (only-in srfi/1 list-index))
  
  (define (ordered? ps chain)
    
    (define (test x)
      
      (define (index y)
        (list-index (⤶ equal? y) chain))
                
      (<= (index (car x))
          (index (cdr x))))
    
    (andmap test (multi-set->list (poset-orders ps))))

  (define example-poset 
    (poset 
     (list (≼ 1 2)  (≼ 1 5)  (≼ 5 7)   (≼ 7 8)   
           (≼ 8 9)  (≼ 9 10) (≼ 18 49) (≼ 29 49)
           (≼ 29 1) (≼ 8 18)))))


#| Ｎｏｄｅｓ |#

;; TODO: use struct

(define (node a b)
  (cons a b))

(define (node-adjacencies x)
  (cdr x))

(define (node-value x)
  (car x))

(define (node-in-degree x)
  (length (cdr x)))

(define (sort-nodes xs)
  (sort xs <
   #:cache-keys? #t
   #:key node-in-degree))


#| Ｐｏｓｅｔ |#

(define (≼ x y) ; ⪯ or ≺ or ≼ ?
  (cons x y))


(define (poset-empty? ps)
  (empty? ps))


(define (poset arrows)

  (define (arrows->objs xys)
    (match-define (cons x y) (unzip xys))
    (append x y))
  
  (define all-id 
    (list->set (map (λ (x) (≼ x x))
                    (arrows->objs arrows))))  
    
  (define missing-id
    (set->list
     (set-subtract all-id
                   (set-intersect (list->set arrows) all-id))))
  
  ((∘ make-immutable-hash group-arrows)
   (append missing-id arrows)))

(module+ test
  (check-equal?
   (poset (list (≼ 1 1) (≼ 1 2) (≼ 1 5) (≼ 3 4)))
   '#hash((2 . (2))
          (4 . (4))
          (5 . (5))
          (1 . (1 2 5))
          (3 . (3 4)))
         
   "poset"))


#| ｐｏｓｅｔ－ｏｒｄｅｒｓ
   convert poset to mult-set of order relationships |#

(define (poset-orders ps)
  
  (define (f x) 
     (map (⤶ ≼ (node-value x)) (node-adjacencies x)))
  
  (list->multi-set
   (apply append
          (map f (hash->list ps)))))

(module+ test
  (let [[xs (list (≼ 1 2) (≼ 1 5) (≼ 3 4)
                  (≼ 1 1) (≼ 2 2) (≼ 3 3)
                  (≼ 1 1) 
                  (≼ 4 4) (≼ 5 5))]]
    (check-equal?
     (poset-orders (poset xs))
     (list->multi-set xs)
     "poset-orders")))



#| ｐｏｓｅｔ－＞ｃｈａｉｎ
   linear extension of a poset (i.e. topological sort) |#

(define (poset->chain objects)
    
  (define/match (pop-min ps)
    [((? empty?)) (void)]
    [(_)
     (define-values (x xs)
       (drop-minimal ps))
        
     (when (< 1 (length (cdr x)))
       (error "poset->chain: cycle detected."))
     
     (cons (car x) xs)])
  
  ;; Since we only store the outgoing edges we need to operate on the dual
  ;; category
  ;; TODO: reverse unfold?
  
  (reverse (unfold pop-min (hash->list objects)))) 

(module+ test
  (check-true
   (ordered? example-poset (poset->chain example-poset))
   "poset->chain"))




#| ｇｒｏｕｐ－ａｒｒｏｗｓ
   convert order relations into an adjacency list of outgoing edges |#

(define (group-arrows lst)
  
  (define (deconstruct xs)
    (cons (caar xs)
          (map cdr xs)))

  (map deconstruct
       (equiv-classes (equal-at? car) lst)))



(module+ test
  (check-equal?
   (group-arrows (list (≼ 1 2) (≼ 1 5) (≼ 3 4)))
   '((1 . (2 5))
     (3 . (4)))
   "group-arrows"))


 
#| ｄｒｏｐ－ｍｉｎｉｍａｌ
   seperate minimal element from rest of set |#

(define (drop-minimal input)
  
  (match-define (cons dropped remaining)
    (sort-nodes input))
  
  (define (cull n)
    (node (node-value n)
          (filter-not (⤶ equal? (node-value dropped))
                      (node-adjacencies n))))
    
  (values dropped
          (map cull remaining)))


