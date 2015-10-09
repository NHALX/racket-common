#lang racket/base
(require racket/function
         racket/port
         racket/match
         racket/string
         racket/set
         racket/list)

(require "../NHA.rkt")

(provide exec)


(define (output-sink-error port)
  ;; displays subproc errors async
  
  (thread (thunk
           (display (port->string port)
                    (current-error-port))
           (close-input-port port))))



(define (shell-pipe-spawn in out commands)
  ;; creates pipeline
  
  (define/match (f cmd rest st)
    [(_ _ (cons xs stdout->stdin))
     
     (define output
       (if (empty? rest)
           out
           #f))         
           
     (define-values (pid stdout _ stderr)
       (apply subprocess (append
                          (list output stdout->stdin #f)
                          (string-split cmd))))
     
     (cons (cons
            (list pid (output-sink-error stderr) stdout)
            xs)
           stdout)])

  (paramorphism f (cons '() in) commands))


(define (shell-pipe-wait results)
  ;; fetches subproc results
  
  (define/match (f _)
    [((list))
     (void)]
    
    [(xs) 
     (let* [[z     (apply sync xs)]
            [error (subprocess-status z)]]
     
     (if (= error 0)
         (cons error (set-remove xs z))
         (begin ; TODO: make sure this kill interacts with groups properly
           (for-each (⤷ subprocess-kill #f) xs)
           (cons error '()))))])
  
  (unfold f (map car results)))



(define (shell-pipe-join-threads results)  
  (for-each (∘ thread-wait cadr) results))



(define (shell-pipe-cleanup results)
  ;; closes subprocess ports
  
  (define (cleanup pid tid port)
    (when port
      (close-input-port port)))
  
  (for-each (⤶ apply cleanup) results))



#| ｅｘｅｃ
   unix-like shell command pipeline with optional input/output
   file-paths or ports |#

(define (exec #:in-fp [in #f]
              #:out-fp [out #f]
              #:exists [exists-flag 'error]
              . commands)

  (define (do-exec i o)
    (match-define (cons results output)
      (shell-pipe-spawn i o commands))
    
    (when output
      (display (port->string output)))
    
    (define success
      (andmap (⤶ = 0)
              (shell-pipe-wait results)))
    
    (shell-pipe-join-threads results) ; prevents race condition when aborting
    (shell-pipe-cleanup results)
    
    (unless success
      (error 'exec "external process signaled failure.")))


  (parameterize ([current-custodian (make-custodian)])
    
    (define (maybe-open x open)
      (cond [(path-string? x)  (open x)]
            [(port? in)        x]
            [else              #f]))
    
    (define in-port
      (maybe-open in open-input-file))
    
    (define out-port
      (maybe-open out (curry open-output-file #:exists exists-flag)))
          
    (do-exec in-port out-port)
    
    (custodian-shutdown-all (current-custodian))))


(module+ test
  
  ; TODO: generic/better test
  
  #;(exec #:in-fp  "/home/nha/.profile"
        #:out-fp "/home/nha/test.txt"
        #:exists 'replace
        "/usr/bin/wc -l"))

