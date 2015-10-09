#lang racket/base
(require racket/string
         racket/match
         racket/path
         racket/list
         racket/bool
         racket/set)

(require syntax/modresolve
         syntax/modcode)

(require "../NHA.rkt")

(require graph)

(provide dependency-graph-resolve
         dependency-graph-subset
         dependency-graph-map)


(module+ test
  (require rackunit))



(define (all-subs target-path)
  ;; query submodules - only goes one level deep
  ;; TODO: recursively collect all subs of subs to arbitrary depth
  
  (let [[code (get-module-code target-path)]]
    (append (module-compiled-submodules code #f)
            (module-compiled-submodules code #t))))


(define (all-imports relative-path module-code)
  ;; query top-level imports which point to files/paths
  
  (define (get-imports code) 
    (concat 
     (map cdr 
          (module-compiled-imports code))))
  
  (define imports
    (map (⤷ resolve-module-path-index relative-path)
         (get-imports module-code)))
  
        
  (define/match (select x)
    [((? path?))                           x]
    [((? symbol?))                         #f]
    [((cons 'submod (cons (? path?) _)))   (cadr x)]
    [((cons 'submod (cons (? symbol?) _))) #f])

  (filter-not false? (map select imports)))



(define (imports:level<0-1> n)
  
  (let
      [(level:0 (all-imports n (get-module-code n)))
       (level:1 (concat-map (⤶ all-imports n)
                            (all-subs n)))]
    (append level:0
            level:1)))


#| ｄｅｐｅｎｄｅｎｃｙ－ｇｒａｐｈ－ｒｅｓｏｌｖｅ
   discover/graph the dependencies of the chosen module-path,
   ignoring those which do not match the given predicate |#

(define (dependency-graph-resolve pred path)
  
  (define root     (resolve-module-path path #f))
  (define visited  (mutable-set))
  (define graph    (directed-graph empty)) 
  
  (define (recurse! n)
    
    (define-values (new old)
      (partition (λ (x) (not (set-member? visited x)))
                 (filter pred
                         (imports:level<0-1> n))))
    #|
    (printf (string-append
             "target: ~a\n"
             "sub: ~a\n"
             "imports: ~a\n"
             "visited: ~a\n")
            n
            (map module-compiled-name (all-subs n))
            (append new old)
            visited)
    |#
    
    (set-add! visited n)
    (for-each (λ (x) (add-directed-edge!
                      graph
                      (simplify-path n)
                      (simplify-path x)))
              
              (append new old))
    
    (for-each recurse! new))

  (recurse! root)
  graph)


(module+ test 
  
  (let*
      [(cwd   (path->string (current-directory)))
       (this  (syntax-source #'#f))
       (pred  (let* [(suffix    (regexp-quote ".rkt"))
                     (prefix    (regexp-quote cwd))
                     (criteria  (regexp (string-append prefix ".*" suffix)))]
                
                (λ (x) (regexp-match? criteria 
                                      (path->string x)))))
       
       (graph (dependency-graph-resolve pred this))

       ;; test related
       (edges (map² (∘ path->string (⤶ find-relative-path cwd))
                    (get-edges graph)))

       (p-key  (λ (x) (string-append (car x) (cadr x))))
       (p-sort (λ (x) (sort x string<? #:key p-key)))]

    (check-equal?
     (p-sort edges)
     (p-sort
      '(("dependency-graph.rkt" "../NHA.rkt")
        ("dependency-graph.rkt" "dependency-graph.rkt")
        ("../NHA.rkt" "../NHA.rkt")))
     "dependency-graph-resolve")))



#| ｄｅｐｅｎｄｅｎｃｙ－ｇｒａｐｈ－ｍａｐ
   functionally transform all graph nodes by f |#

(define (dependency-graph-map f input)

  (define graph
    (graph-copy input))
    
  (for-each (λ (v)
              (let [[v2 (f v)]]
                (unless (equal? v2 v)
                  (rename-vertex! graph v (f v)))))
              (get-vertices graph))
  graph)

(module+ test
  (let [[g1 (directed-graph '((1  2)))]
        [g2 (directed-graph '((10 20)))]]
    
    (check-equal?
     (get-edges (dependency-graph-map (λ (x) (* 10 x)) g1))
     (get-edges g2)
     "dependency-graph-map")))



#| ｄｅｐｅｎｄｅｎｃｙ－ｇｒａｐｈ－ｓｕｂｓｅｔ
   subset of all dependencies that are required by the
   given node |#

(define (dependency-graph-subset graph src)
    (do-bfs graph src
                  #:init null
                  #:on-enqueue: (cons $v $acc)))


(module+ test 
  
  (let* [[graph (directed-graph '((A B) (B C) (B D)))]]
    
    (check-equal?
     (sort (dependency-graph-subset graph 'A)
           string<? #:key symbol->string)
     '(B C D)
     "dependency-graph-subset")))



#| ｄｅｐｅｎｄｅｎｃｙ－ｇｒａｐｈ－＞ｍａｔｈｅｍａｔｉｃａ－ｇｒａｐｈ
   output dependency graph as a Wolfram Mathematica graph |#

(define (dependency-graph->mathematica-graph cwd graph)
  
  (define edges
    (map
     (λ (x)
       (format "DirectedEdge[\"~a\", \"~a\"]"
               (car x)
               (cadr x)))
     (get-edges
      (dependency-graph-map
       (⤶ find-relative-path cwd)
       graph))))

  (format "Graph[{~a}, VertexLabels -> \"Name\"]"
          (string-join edges ",\n")))


(module+ test
  (let
      [[expected
        (unlines
         (list
          "Graph[{DirectedEdge[\"A\", \"B\"],"
          "DirectedEdge[\"A\", \"C\"]}, VertexLabels -> \"Name\"]"))]
       
       [g (directed-graph '(("/root/A" "/root/B")
                            ("/root/A" "/root/C")))]]
    
    (check-equal?
     (dependency-graph->mathematica-graph "/root" g)
     expected
     "dependency-graph->mathematica-graph")))
