#lang racket/base

(module+ test (require rackunit))
(require syntax/parse (for-syntax syntax/parse racket/base))
(require racket/undefined
         racket/function
         racket/string
         racket/match
         rackunit)

(provide do: >>= return
         current:>>=
         current:return
         law:associativity
         law:left-identity
         law:right-identity)


#| Ｍｏｎａｄ |#

(define (>>= . xs)   (apply (current:>>=)    xs))
(define (return . x) (apply (current:return) x))

(define current:>>=     (make-parameter undefined))
(define current:return  (make-parameter undefined))


#| Ｍｏｎａｄ Ｌａｗｓ |#

(define (test-f x)
  (return (+ 1 x)))
  
(define (test-g x)
  (return (/ 7 x)))

(define (test-m)
  (return 7))

(define-syntax (law:left-identity stx)
  (syntax-parse stx
    [(_ monad ...)
     #'(check-equal?
        (monad ... : (>>= (test-m) test-f))
        (monad ... : (test-f 7)))]))

(define-syntax (law:right-identity stx)
  (syntax-parse stx
    [(_ monad ...)
     #'(check-equal?
        (monad ... : (>>= (test-m) return))
        (monad ... : (test-m)))]))

(define-syntax (law:associativity stx)
  (syntax-parse stx
    [(_ monad ...)
     #'(check-equal?
        (monad ... : (>>= (>>= (test-m) test-f) test-g))
        (monad ... : (>>= (test-m)
                          (λ xs
                            (>>= (apply test-f xs) test-g)))))]))


#| ｄｏ Ｎｏｔａｔｉｏｎ |#

(define (fixed-arg-length xs)
  
  (foldl (λ (x n)
           ((if (keyword? x) - +) n 1))
         0 xs))


(define-syntax-rule (curry* f xs ...) ; only curries when missing args

  (if (procedure? f)        
      (if (arity-includes? (procedure-arity f)
                           (fixed-arg-length '(xs ...)))
          (f xs ...)
          (curry f xs ...))
      f))


(begin-for-syntax
  (define-splicing-syntax-class command
    (pattern
     (~or
      ;; liftM support
      ((~seq (~or
              (~datum liftM)
              (~datum ↑))
             lift-f
             (~and (cmd ...)
                   (~bind
                    [wrapped #'(do: x ← (cmd ...)
                                    (return (lift-f x)))]))))
      ;; normal action
      (~and (cmd ...)
            (~bind [wrapped #'(curry* cmd ...)])) ))))


(define-syntax (do: stx)
  (syntax-parse stx
        
    [(_ (~seq (~seq xs:id ...+ (~or
                                (~datum ←)
                                (~datum <-))
                    action:command))
        (~describe
         "result: last statement in a do block must be an expression."
         (~seq ~! next ...+)))
     
     #`(>>= action.wrapped (λ (xs ...) (do: next ...)))]
    
    [(_ action:command next ...+)
     #`(>>= action.wrapped (λ (_) (do: next ...)))]

    [(_ action:command)
     #`action.wrapped]))

