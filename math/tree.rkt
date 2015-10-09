#lang racket/base
(require "../NHA.rkt")
(require racket/match
         racket/function
         racket/list
         racket/string
         racket/promise)

(provide (struct-out tree)
         tree-map
         tree-find
         tree-ref
         tree-insert
         tree-insert!
         tree-edges tree-nodes tree-unfold
         tree->mathematica-graph)

(module+ test
  (require racket/list)
  (require rackunit))



#| ｔｒｅｅ
   constructor |#

(struct tree (value children)
  #:transparent
  #:mutable)


(module+ test  
  (define test-data:tree1
    (tree 1 (list
             (tree 2 (list (tree 3 empty)))
             (tree 4 empty)))))


#| ｔｒｅｅ－ｍａｐ
   transform node values |#

(define (tree-map f n)
  (tree (f (tree-value n))
        (map (⤶ tree-map f) (tree-children n))))
           
(module+ test
  (define test-data:tree2
    (tree-map (⤶ cons 'a) test-data:tree1)))
  

#| ｔｒｅｅ－ｎｏｄｅｓ
   collect tree nodes as list |#

(define (tree-nodes root #:depth-first [dfs #f])
  (tree-unfold #:depth-first dfs
   (λ (x) (values (tree-value x)
                   (tree-children x))) root))

(module+ test
  (check-equal? (tree-nodes test-data:tree1 #:depth-first #t)
                (list 1 2 3 4))
  
  (check-equal? (tree-nodes test-data:tree1)
                (list 1 2 4 3)))


#| ｔｒｅｅ－ｅｄｇｅｓ
   collect tree edges as list |#

(define (tree-edges root)
  
  (define (g n)
    
    (values
     (map (λ (x)
            (cons (tree-value n)
                  (tree-value x)))
          (tree-children n))
     
     (tree-children n)))
  
  (apply append (tree-unfold g root)))

(module+ test
  (check-equal?
   (tree-edges (tree-map add1 test-data:tree1))
   '((2 . 3) (2 . 5) (3 . 4)))
  
  (check-equal?
   (tree-edges test-data:tree1)
   '((1 . 2) (1 . 4) (2 . 3))))



#| ｔｒｅｅ－ｕｎｆｏｌｄ
   anamorphism: apply f to nodes and collect results |#

(define (tree-unfold f root #:depth-first [dfs #f])
  
  (define/match (g _)
    [((list)) (void)]
    [((cons x queue))

     (define-values (val next)
       (f x))
     
     (cons val
           (if dfs
               (append next queue)
               (append queue next)))])
  
  (unfold g (list root)))

(module+ test
  (check-equal?
   (tree-unfold
    #:depth-first #t
    (λ (n) (values (tree-value n)
                   (tree-children n)))
    test-data:tree1)
   
   '(1 2 3 4)))


#| ｔｒｅｅ－ｆｉｎｄ
   search for node in tree |#

;; TODO: fix position of root arg on all functions above this line

(define (tree-find v root)
  (if (equal? v (tree-value root))
      root
      (for/or [[x (tree-children root)]]
        (tree-find v x))))

(module+ test
  (check-equal?
   (tree-find 2 test-data:tree1)
   (tree 2 (list (tree 3 empty)))))



#| ｔｒｅｅ－ｒｅｆ
   get node indexed by path |#

(define (tree-ref root path #:key [--> identity])      
  (define/match (f path-list node)
    [((cons x (list)) (tree (app --> x) __)) node]
    [((cons x xs)     (tree (app --> x) ns)) (ormap (⤶ f xs) ns)]
    [(_ _)                                   #f])
   (f path root))

(module+ test
  (check-equal? (tree-ref test-data:tree1 (list 1 2 3))
                (tree 3 empty)))



#| ｔｒｅｅ－ｉｎｓｅｒｔ
   functional update |#

(define (tree-insert root path new #:key [--> identity])
  
  (define (update-list xs x)
    (define-values (l r)
      (splitf-at xs
                 (λ (y)
                   (not (equal? (--> (tree-value x))
                                (--> (tree-value y)))))))
    (if (or (empty? l) (empty? r))
        (append xs (list x))
        (append l (cons x (cdr r)))))

  (define/match (f key-list n)
    [((cons x (list)) (tree (app --> x) ns))
     (tree (tree-value n)
           (update-list ns (tree new empty)))]
    
    [((cons x xs) (tree (app --> x) ns))
     (tree (tree-value n)
           (map (⤶ f xs) ns))]
    
    [(_ node)
     node])
  
   (f path root))


(module+ test
  (check-equal?
   (tree-edges
    ((∘ (⤷ tree-insert (list 1 2) '(a . 999) #:key cdr)
        (⤷ tree-insert (list 1 2) '(a . 999) #:key cdr)
        (⤷ tree-insert (list 1 2) '(a . 999) #:key cdr))
     test-data:tree2))
   '(((a . 1) . (a . 2))
     ((a . 1) . (a . 4))
     ((a . 2) . (a . 3))
     ((a . 2) . (a . 999)))))



#| ｔｒｅｅ－ｉｎｓｅｒｔ！
   in-place mutating update |#

(define (tree-insert! root path new #:key [--> identity])
  
  (define subtree
    (tree-ref root path #:key -->))
  
  (define existing
    (delay
      (tree-ref subtree
                (list (--> (tree-value subtree))
                      (--> new))
                #:key -->)))
  
  (cond
    [(and subtree (force existing))
          (set-tree-value! (force existing) new)]

    [subtree
     (set-tree-children! subtree
                         (append (tree-children subtree)
                                 (list (tree new empty))))]
    [else
     (error 'tree-insert! "key not found: ~a" path)]))


(module+ test
  (define m-tree (tree '(a . 1) empty))
  
  (tree-insert! m-tree (list 1) '(b . 2) #:key cdr)
  (tree-insert! m-tree (list 1) '(b . 2) #:key cdr)
  (tree-insert! m-tree (list 1) '(b . 2) #:key cdr)
  
  (check-equal? (tree-edges m-tree)
                '(((a . 1) . (b . 2)))))



#| ｔｒｅｅ－＞ｍａｔｈｅｍａｔｉｃａ－ｇｒａｐｈ
   output tree as a Wolfram Mathematica graph |#

(define (tree->mathematica-graph t)
  (define edges
    (map
     (λ (x)
       (format "DirectedEdge[\"~a\", \"~a\"]"
               (car x)
               (cdr x)))
     (tree-edges t)))

   (format "Graph[{~a}, VertexLabels -> \"Name\"]"
           (string-join edges ",\n")))


(module+ test
  (check-equal?
   (tree->mathematica-graph test-data:tree1)
   (string-join
    (list "Graph[{DirectedEdge[\"1\", \"2\"],"
          "DirectedEdge[\"1\", \"4\"],"
          "DirectedEdge[\"2\", \"3\"]}, VertexLabels -> \"Name\"]")
    "\n")))


 
