#lang racket
(require racket/undefined)
(require syntax/parse (for-syntax syntax/parse racket/syntax))
(require "math/monad.rkt"
         "math/monad/eff.rkt")
(require "NHA.rkt")
(require racket/stxparam)

(provide history-describe define:re-entrant do:re-entrant)

(module+ test
  (require rackunit))


(struct historical-event
  (travel symbol callback-description)
  #:transparent)



;; backlog of composed historical events
;; (available within define-re-entrant blocks)

(define-syntax-parameter re-entrant:history '()) 


#| ｄｅｆｉｎｅ：ｒｅ－ｅｎｔｒａｎｔ
   define a function, enhancing it for use in the re-entrant monad |#

(define-syntax (define:re-entrant stx)

  (syntax-parse stx
    
    [(_ (func-name xs ...)
        (~optional
         ((~datum retry-callback) callback-description callback)
         #:defaults [[callback             #'(void)]
                     [callback-description #'#f]])
        body ...+)
     
     #`(define (func-name xs ...)
         
         (define (action)
           
           (define event
             (historical-event
              (call/cc
               (λ (k)
                 (letrec [[g (λ (use-callback?)
                               (when use-callback? callback)
                               (k g))]]
                   g)))
              'func-name
              callback-description))
           
           (define (function-body h)
             (syntax-parameterize
                 [[re-entrant:history (make-rename-transformer #'h)]]
               body ...))
           
           (do:
               (modify (curry cons event))
               history <- (get)
               (return (function-body history))))
         
         (action))]))


#| ｄｏ：ｒｅ－ｅｎｔｒａｎｔ
   monadic composition of re-entrant functions |#

(define-syntax-rule (do:re-entrant xs ...)
  (monad/eff
   (runIdentity)
   (evalState empty)
   ----
   xs ...))


#| ｇｅｔ－ｈｉｓｔｏｒｙ
   return the current history backlog |#

(define:re-entrant (get-history)
  re-entrant:history)


#| ｈｉｓｔｏｒｙ－ｄｅｓｃｒｉｂｅ
   pretty-print last n historical events |#

(define (history-describe history [end (length history)])
  (define callback
    (historical-event-callback-description
     (list-ref history (- end 1))))
  
  (define backlog
    (reverse (map historical-event-symbol
                  (slice history 0 end))))
  (join
   (if callback
       (cons callback backlog)
       backlog)
   " >> "))


(module+ test
  
  (define:re-entrant (f) "f")
  (define:re-entrant (g) "g")
  (define:re-entrant (h) "h")
  
  (check-equal?
   (history-describe
    (do:re-entrant
     (f)
     (g)
     (h)
     (get-history)))
   "f >> g >> h >> get-history"
   "history-describe"))


#| ｈｉｓｔｏｒｙ－ｒｅｗｉｎｄ
   jump to a previous state specified by a backlog index or symbol name |#

(define (history-rewind history target [use-callback? #f])
  (define event
    (if (symbol? target)
        (findf (∘ (⤶ equal? target) historical-event-symbol) history)
        (list-ref history target)))
  
  ((historical-event-travel event)
   use-callback?))



(module+ test

  (define log
    (open-output-string))
  
  (define jump-at-pass
    (let [[pass 0]]
      (λ (condition history index)
        (when (equal? pass condition)
          (set! pass (+ 1 pass))
          (printf "!! jump to: ~a\n" index)
          (history-rewind history index #t)))))

  
  (define (edit x)
    (printf "<< editing: ~a\n" x))

  
  (define:re-entrant (generate file)
    (retry-callback "edit1" (edit file))
    
    (printf "++ generate: ~a\n" file)
    (jump-at-pass 3 re-entrant:history 'generate)
    "source.c")

  
  (define:re-entrant (compile file)
    (retry-callback "edit2" (edit file))
        
    (printf "++ compile: ~a\n" file)
    
    (jump-at-pass 0 re-entrant:history 'generate)

    "test.exe")

  
  (define:re-entrant (run file)
    (printf "++ run: ~a\n" file)
    (jump-at-pass 1 re-entrant:history 'compile)
    (jump-at-pass 2 re-entrant:history 'generate)
    (jump-at-pass 4 re-entrant:history 'run))


  (parameterize [[current-output-port (open-output-string)]]
    (check-equal?
     
     (begin
       (do:re-entrant
        src <- (generate "123")
        exe <- (compile src)
        (run exe))
       (get-output-string (current-output-port)))
     
     (unlines
      (list
       "++ generate: 123"
       "++ compile: source.c"
       "!! jump to: generate"
       "<< editing: 123"
       "++ generate: 123"
       "++ compile: source.c"
       "++ run: test.exe"
       "!! jump to: compile"
       "<< editing: source.c"
       "++ compile: source.c"
       "++ run: test.exe"
       "!! jump to: generate"
       "<< editing: 123"
       "++ generate: 123"
       "!! jump to: generate"
       "<< editing: 123"
       "++ generate: 123"
       "++ compile: source.c"
       "++ run: test.exe"
       "!! jump to: run"
       "++ run: test.exe"
       "")))))

