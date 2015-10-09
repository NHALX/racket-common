#lang racket/base
(require racket/match
         racket/list
         racket/format
         racket/string
         racket/function
         racket/contract
         racket/bool)

(require syntax/parse
         (for-syntax syntax/parse
                     racket/syntax
                     racket/base))

(provide (all-defined-out)
         (for-syntax all-defined-out))


(module+ test (require rackunit))



#|||
┏━┓┏━┓╺┳╸╻ ╻
┣━┛┣━┫ ┃ ┣━┫
╹  ╹ ╹ ╹ ╹ ╹
|||#

#| ｄｉｒｅｃｔｏｒｙ－ｆｒｏｍ－ｐａｔｈ
   extract base directory from file path |#

(define (directory-from-path p)
  (define/contract (f p)
    (-> path-string? (values path? path? false?))
    (split-path (path->complete-path p)))
  (value:0 (f p)))


(module+ test
  (check-equal?
   (directory-from-path "x/test")
   (path->complete-path "x/")))




#|||
╻  ╻┏━┓╺┳╸
┃  ┃┗━┓ ┃ 
┗━╸╹┗━┛ ╹
|||#

#| ｓｌｉｃｅ
   extract bounded subsequence from list |#

(define (slice lst start end)
  (take (drop lst start) (- end start)))

(module+ test
  (check-equal? (slice '(1 2 3 4) 1 3)
                '(2 3)
                "slice"))


#| ｃｏｎｃａｔ
   flatten one level of nested lists |#

(define (concat xs)
  (apply append xs))

(module+ test
  (check-equal? (concat (list (list 1) (list 2)))
                (list 1 2)
                "concat"))


#| ｍａｐⁿ
   nested structural maps |#

(define (map² f . xs)
  (apply map (λ x
               (apply map f x)) xs))

(define (map³ f . xs)
  (apply map (λ x
               (apply map (λ z
                            (apply map f z)) x)) xs))

(define (concat-map f . xs)
  (concat (apply map f xs)))


(module+ test
  (check-equal?
   (map² add1 (list (list 1 2)
                    (list 3 4)))
   (list (list 2 3)
         (list 4 5)))

  (check-equal?
   (map³ add1 (list (list (list 1))))
   (list (list (list 2))))

  (check-equal?
   (concat-map (λ (xs) (map add1 xs)) '((1 2) (3)))
   '(2 3 4)))



#| ｕｎｚｉｐ
   seperate a list of pairs into two lists |#

(define (unzip lst)
  
  (match lst
    ['()                  (cons '() '())]
    [(cons (cons a b) tl) (define xs (unzip tl))
     (cons (cons a (car xs))
           (cons b (cdr xs)))]))

(module+ test
  (check-equal?
   (unzip '((1 . a) (2 . b)))
   '((1 2) . (a b))
   "unzip"))



#| ｅｑｕｉｖ－ｃｌａｓｓｅｓ
   partition input list based on predicate |#

(define (equiv-classes cmp lst)
  
  (define/match (split xs)

    [((list))
     (void)]
    
    [((cons target _))
     
     (define-values (eqv un)
       (partition (⤶ cmp target) xs))
     
     (cons eqv un)])
  
  
  (unfold split lst))

(module+ test
  (check-equal?
   (equiv-classes equal? '(1 1 1 3 4 5))
   '((1 1 1) (3) (4) (5))
   "equiv-classes"))



#|||
┏━┓╺┳╸┏━┓╻┏┓╻┏━╸
┗━┓ ┃ ┣┳┛┃┃┗┫┃╺┓
┗━┛ ╹ ╹┗╸╹╹ ╹┗━┛
|||#

#| ｊｏｉｎ
   generic string-join by converting inputs via ~a |#

(define (join xs [sep " "]	 	 	 
              #:before-first [before-first ""]
              #:before-last [before-last sep]
              #:after-last [after-last ""])
  
  (string-join (map ~a xs) sep
               #:before-first before-first
               #:before-last before-last
               #:after-last after-last))

(module+ test
  (check-equal?
   (join (list 'a "b" 1 2) " ")
   "a b 1 2"))


#| ｕｎｌｉｎｅｓ
   flatten list of strings with "\n" |#

(define (unlines xs)
  (string-join xs "\n")) 

(module+ test
  (check-equal? (unlines (list "a" "b"))
                "a\nb"))


#| ｓｔｒｉｐ－ｎｅｗｌｉｎｅ
   drop ending newline from structures that scribble/output understands:
    * list
    * string
    * char |#

(define (strip-newline x)
  
  (cond
    [(char? x)    (if (char-whitespace? x) "" x)] ; TODO: huh?
    [(string? x)  (string-trim x #px"\\s*" #:left? #f)]
    [(list? x)    (let-values [[(l r) (split-at-right x 1)]]
                    (append l (list (strip-newline (car r)))))]
    [else x]))

(module+ test
  (check-equal?
   (strip-newline "123 \n") "123")

  (check-equal?
   (strip-newline (list "1234 \n"))
   (list "1234")))


#|||
┏━╸╻ ╻┏┓╻┏━╸╺┳╸╻┏━┓┏┓╻
┣╸ ┃ ┃┃┗┫┃   ┃ ┃┃ ┃┃┗┫
╹  ┗━┛╹ ╹┗━╸ ╹ ╹┗━┛╹ ╹
|||#

#| ｓｙｍｂｏｌ： ⤶
   left curry
   (⤶      f x) --> (λ (y) (f x y))
   (⤶ : T  f x) --> (λ ([y : T]) (f x y)) |#

(define-syntax (⤶ stx)
  (syntax-case stx (:)
    [(_ : type f x) 
     #`(λ ([y : type]) (f x y))]
    [(_ f rest ...) 
     #`(λ (y) (f rest ... y))]))

(module+ test
  (check-equal? ((⤶ format "~a-~a-~a-~a\n" 1 2 3) 4)
                "1-2-3-4\n")
  (check-equal? ((⤶ format) "hi")
                "hi"))


#| ｓｙｍｂｏｌ： ⤷
   right curry
   (⤷      f y) --> (λ (x) (f x y))
   (⤷ : T  f y) --> (λ ([x : T]) (f x y)) |#

(define-syntax (⤷ stx)
  (syntax-case stx (:)
    [(_ : type f y) 
     #`(λ ([x : type]) (f x y))]
    [(_ f rest ...) 
     #`(λ (x) (f x rest ...))]))


(module+ test
  (check-equal? ((⤷ format 1 2 3 4) "~a-~a-~a-~a\n")
                "1-2-3-4\n")
  (check-equal? ((⤷ format) "hi")
                "hi"))


#| ｓｙｍｂｏｌ： ∘   
   function composition
   (∘ f g h)            --> (λ (x) (f (g (h x))))
   (∘ : T1 → T2  f g h) --> (λ ([x : T1]) : T2
                                   (f (g (h x)))) |#

(define-syntax (∘ stx)
  (define (nest stx-x)
    (syntax-parse stx-x
      [(_ f)            #'(apply f args)]
      [(_ f fs ...+)    #`(f #,(nest #'(_ fs ...)))]))
  
  #`(λ args #,(nest stx)))

(module+ test
  (check-equal? ((∘ add1 identity) 1) 2)
  (check-equal? ((∘ add1 (const 1))) 2))


#| ｓｙｍｂｏｌ： ↫
   projected binary application |#

(define (↫ f . ps)
  (define p (apply compose ps))
  (λ (x y) (f (p x) (p y))))

(define (equal-at? x)
  (↫ equal? x))

(module+ test
  (check-equal?
   ((equal-at? car)
    '(a 1)
    '(a 2))
   #t
   "equal-at?"))


#|||
┏━┓┏━╸┏━╸╻ ╻┏━┓┏━┓╻┏━┓┏┓╻
┣┳┛┣╸ ┃  ┃ ┃┣┳┛┗━┓┃┃ ┃┃┗┫
╹┗╸┗━╸┗━╸┗━┛╹┗╸┗━┛╹┗━┛╹ ╹
|||#


#| ｐａｒａｍｏｒｐｈｉｓｍ
   https://en.wikipedia.org/wiki/Paramorphism |#

(define/match (paramorphism f n _)
  [(_ _ (list)) n]
  [(_ _ (cons x xs)) 
   (f x xs
      (paramorphism f n xs))])

(module+ test

  (check-equal?
   (map list->string
        (paramorphism
         (λ (_ xs suffixes) (cons xs suffixes))
         empty
         (string->list "suffix")))
   
   '("uffix" "ffix" "fix" "ix" "x" "")))


#| ｕｎｆｏｌｄ
   anamorphism
   (similar to the haskell version) |# 

(define (unfold f seed [tail-gen (lambda (x) '())])
  (let [[x (f seed)]]
    (if (void? x)
        (tail-gen seed)
        (cons (car x)
              (unfold f (cdr x))))))

(module+ test
  
  (define (unfold-test n)
    (if (= n 10)
        (void)
        (cons n (+ 1 n))))
  
  (check-equal?
   (unfold unfold-test 0)
   '(0 1 2 3 4 5 6 7 8 9)
   "unfold"))



#|||
┏┳┓╻┏━┓┏━╸
┃┃┃┃┗━┓┃  
╹ ╹╹┗━┛┗━╸
|||#


#| ｖａｌｕｅ：ｎ
   extract results from (values) by index |#

(define-syntax-rule (value:0 expr)
  (call-with-values (λ () expr) (λ (v1 . rest) v1)))

(define-syntax-rule (value:1 expr)
  (call-with-values (λ () expr) (λ (v1 v2 . rest) v2)))

(define-syntax-rule (value:2 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 . rest) v3)))

(define-syntax-rule (value:3 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 v4 . rest) v4)))

(define-syntax-rule (value:4 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 v4 v5 . rest) v5)))

(define-syntax-rule (value:5 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 v4 v5 v6 . rest) v6)))

(define-syntax-rule (value:6 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 v4 v5 v6 v7 . rest) v7)))

(define-syntax-rule (value:7 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 v4 v5 v6 v7 v8 . rest) v8)))

(define-syntax-rule (value:8 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 v4 v5 v6 v7 v8 v9 . rest) v9)))

(define-syntax-rule (value:9 expr)
  (call-with-values (λ () expr) (λ (v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 . rest) v10)))


(module+ test 
  (check-equal? (value:0 (values 1 2))
                1))




#|||
┏┳┓┏━┓┏━╸┏━┓┏━┓   ╻ ╻╺┳╸╻╻  
┃┃┃┣━┫┃  ┣┳┛┃ ┃╺━╸┃ ┃ ┃ ┃┃  
╹ ╹╹ ╹┗━╸╹┗╸┗━┛   ┗━┛ ╹ ╹┗━╸
|||#

#| ｇｅｎｅｒａｔｅ－ｎ－ｔｅｍｐｏｒａｒｉｅｓ
   generate a sequence of n temporary symbols
   TODO: delete this? |#

(define-for-syntax (generate-n-temporaries stx)
  (generate-temporaries (build-list (syntax->datum stx) (λ (i) stx))))



