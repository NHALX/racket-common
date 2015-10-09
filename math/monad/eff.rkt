#lang racket
(require (for-syntax syntax/parse))
(require racket/generic)

(require "../monad.rkt")
(provide monad/eff admin send define/dispatch
         modify local get put runState evalState execState 
         ask runReader
         runIdentity)


#| Ｅｆｆｅｃｔ Ｍｏｎａｄ 

see: http://okmij.org/ftp/Haskell/extensible/exteff.pdf

[3.1 Reader Effect as an Interaction with a Coroutine]
  ...
  We view effects as arising from communication
  between a client and an effect handler, or authority. This clienthandler
  communication adheres to a generic client-server communication
  model, and thus may be easily modeled as a coroutine: a
  computation sends a request and suspends, waiting for a reply; a
  handler waits for a request, handles what it can, and resumes the
  client. We use the continuation monad to implement such coroutines: |#

(define-generics effect
  (fmap u effect))

(struct Eff (run) #:transparent)

(define (eff:return x)
  (Eff (λ (k) (k x))))

(define (eff:>>= m f)
  (Eff (λ (k)
         ((Eff-run m)
          (λ (v)
            ((Eff-run (f v)) k))))))
  
(define-syntax (monad/eff stx)
  (syntax-parse stx
    [(_ (cs ...) ... (~datum ----) xs ...+)
     #'(parameterize [[current:>>=    eff:>>=]
                      [current:return eff:return]]
         ((compose (curry cs ...) ...) (do: xs ...)))]))


#|...
  The answer type shows that a computation may produce a
  value ... or send a request to read ...
  This request, when resumed, continues the computation,
  which then recursively produces another answer...

  The function admin launches a coroutine with an initial
  continuation expecting a value, which, unless the computation
  diverges, must be the ultimate result.

  ...
  The function send dispatches these requests
  and waits for a reply. It obtains the suspension k of the current
  computation (a return address ...), passes k to the
  user-specified request builder f obtaining the request body ...
  incorporates it into the request ... and delivers it
  to the waiting admin. |#

(define (admin m)
  ((Eff-run m) (λ (v) (cons 'Value v))))

(define (send f)
  (Eff (λ (k) (cons 'Effect (f k)))))


(define-syntax (define/dispatch stx)
  (syntax-parse stx
    [(_ (name args ... m)
        : accept?
        (~datum End)    end-a (~datum =>) end-body
        (~datum Accept) acc-a (~datum =>) accept-body
        (~datum Ignore) ign-a (~datum =>) ignore-body)
     
     #'(define (name args ... m)
       (match m
         [(cons 'Value x)              ((λ end-a end-body)    x)]
         [(cons 'Effect (? accept? k)) ((λ acc-a accept-body) k)]
         [(cons 'Effect u)             (>>= (send (λ (k) (fmap k u)))
                                            (λ ign-a ignore-body))]))]))


#| Ｒｅａｄｅｒ Ｍｏｎａｄ |#

(struct reader (id next)
  #:transparent
  #:methods gen:effect
  [(define (fmap k e)
     (reader (reader-id e)
             (compose k (reader-next e))))])


(define (reader-effect? id t)
  (and (reader? t)
       (equal? id (reader-id t))))


(define (ask [id 0])
  (send (λ (k) (reader id k))))


(define (runReader e m #:identifier [id 0])

  (define/dispatch (loop xxx) : (curry reader-effect? id)
    End    (x) => (return x)
    Accept (r) => (loop ((reader-next r) e))
    Ignore (r) => (loop r))
            
  (loop (admin m)))


(define (local f m [id 0])

  (define/dispatch (loop xxx) : (curry reader-effect? id)
    End    (x) => (return x)
    Accept (r) => (do: e ← (liftM f (ask id))
                      (loop ((reader-next r) e)))
    Ignore (r) => (loop r))
  
  (loop (admin m)))


#| ﻿Ｉｄｅｎｔｉｔｙ Ｍｏｎａｄ |#

(define (runIdentity m)
  (match (admin m)
    [(cons 'Value x) x]))


#|﻿ Ｓｔａｔｅ Ｍｏｎａｄ |#

(struct state (id t next)
  #:transparent
  #:methods gen:effect
  [(define (fmap k s)
     (state (state-id s)
            (state-t  s)
            (compose k (state-next s))))])

(define (state-effect? id t)
  (and (state? t)
       (equal? id (state-id t))))


(define (put s)
  (modify (const s)))


(define (modify f [id 0])
  (send (λ (k)
          (state id f k)))) ; TODO: (const (void)) ?
                

(define (get [id 0])
  (send (λ (k)
          (state id identity (identity k)))))


(define (runState s:0 m #:identifier [id 0])
  
  (define/dispatch (loop s:1 xxx) : (curry state-effect? id)
    End    (x)  => (return (cons s:1 x))
    Accept (st) => (let [[s:2 ((state-t st) s:1)]]
                     (loop s:2 ((state-next st) s:2)))
    Ignore (st) => (loop s:1 st))
    
  (loop s:0 (admin m)))


(define (evalState s m #:identifier [id 0])
  (do: (↑ cdr (runState s m #:identifier id))))


(define (execState s m #:identifier [id 0])
  (do: (↑ car (runState s m #:identifier id))))


#| Ｔｅｓｔ |#

(module+ test
  (require rackunit)
  
  (define out (open-output-string))
  
  (define (display-env)
    (do:
        e2 <- (ask 'Outer)
        (return (displayln e2 out))))

  (check-equal?
   (monad/eff
     (runIdentity)
     (runState "hxx12")
     (runReader -300 #:identifier 'Outer)
     (runReader  100 #:identifier 'Inner)
     ----
     (put "xxyy1414141")
     outer ← (ask 'Outer)
     inner ← (ask 'Inner)
     (local (λ (e2) (+ 7 e2)) (display-env) 'Outer)
     (return (list 'results outer inner)))
   
   (list "xxyy1414141" 'results -300 100))

  (check-equal? (get-output-string out)
                "-293\n"))


