;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 11, Problem 2 ==

; Consider the following the following definition of a graph:

(define-struct graph [v e])
 
; A Graph is a (make-graph [List-of Nat] [Nat Nat -> Boolean])
; Interpretation: the vertices (nodes) and edges in a graph,
; where each vertex is identified by a unique natural number.
; All of the numbers in vertices are assumed to be unique and
; both inputs to the edges function are assumed to be
; valid vertices.

; Note: Below, we'll refer to inputs to the function in order as
; (s)ource then (d)estination.

(define G-2-1
  (make-graph
   (list 3 7)
   (λ (s d)
     (cond [(= s 3) #true]
           [(= s 7) #false]))))

(define G-2-2
  (make-graph
   (list 3 7)
   (λ (s d)
     (cond [(= s 3) (= d 7)]
           [(= s 7) (= d 3)]))))

(define G-3-1
  (make-graph
   (build-list 3 add1)
   (λ (s d)
     (cond [(= s 1) (= d 2)]
           [(= s 2) (= d 1)]
           [(= s 3) #false]))))

(define G-3-2
  (make-graph
   (build-list 3 add1)
   (λ (s d)
     (cond [(= s 1) (or (= d 2)
                        (= d 3))]
           [(= s 2) (or (= d 1)
                        (= d 3))]
           [(= s 3) (or (= d 1)
                        (= d 2))]))))

(define G-5-1
  (make-graph
   (build-list 5 add1)
   (λ (s d)
     (cond [(= s 1) (not (= d 1))]
           [(= s 2) (= d 3)]
           [(= s 3) #false]
           [(= s 4) (= d 5)]
           [(= s 5) (= d 4)]))))

(define G-5-2
  (make-graph
   (build-list 5 add1)
   (λ (s d)
     (cond [(= s 1) (or (= d 2)
                        (= d 4))]
           [(= s 2) (= d 5)]
           [(= s 3) (= d 5)]
           [(= s 4) #false]
           [(= s 5) (= d 3)]))))

(define (graph-temp g)
  (... (lon-temp (graph-v g)) ...
       (graph-e g) ...))



; TODO #1: design the function lonely? that takes a graph and a vertex and determines
; if no edge exists with the supplied vertex as the destination (excluding those where
; the source is also the supplied vertex). You have been supplied some tests for clarity.


(check-expect
 (lonely? G-2-1 3)
 #true)

(check-expect
 (lonely? G-2-2 3)
 #false)

(check-expect
 (lonely? G-3-1 3)
 #true)

(check-expect
 (lonely? G-5-1 1)
 #true)

(check-expect
 (lonely? G-5-1 2)
 #false)


; lonely? : Graph Nat -> Boolean
; determines if no edge exists with the supplied vertex as the destination
(define (lonely? g d)
  (andmap (lambda (s) (or (= s d) (not ((graph-e g) s d)))) (graph-v g)))
  


; TODO #2: design the function is-tight? that takes a graph and a list of vertices from that graph
; and determines if all the supplied vertices are the source for edges whose destinations are all
; other vertices in the list (they may still be connected to other vertices not in the list).
; Another way of saying this is that all the supplied vertices are adjacent to each other, that
; they form a clique (https://en.wikipedia.org/wiki/Clique_(graph_theory)).
; You have been supplied some tests for clarity.

(check-expect
 (is-tight? G-2-1 (list 3))
 #true)

(check-expect
 (is-tight? G-2-1 (list 7))
 #true)

(check-expect
 (is-tight? G-2-1 (list 3 7))
 #false)

(check-expect
 (is-tight? G-2-2 (list 3 7))
 #true)

(check-expect
 (is-tight? G-3-1 (list 1 2))
 #true)

(check-expect
 (is-tight? G-3-1 (list 1 2 3))
 #false)

(check-expect
 (is-tight? G-3-2 (list 1 2))
 #true)

(check-expect
 (is-tight? G-3-2 (list 1 2 3))
 #true)

; is-tight? : Graph [List-of Nat] -> Boolean
; takes a graph and a list of vertices from that graph and determines if all the supplied vertices are
; the source for edges whose destinations are all other vertices in the list
(define (is-tight? g lov)
  (andmap (lambda (s) (andmap (lambda (d) (or (= s d) ((graph-e g) s d))) lov)) lov))

; TODO #3: design the function adjacency-matrix that takes a graph and produces a
; matrix (a list of lists) where each row represents a vertex, each column also
; represents a vertex, and if (row, column) is a 1, then there is an edge from the
; vertex of the row to the vertex of the column.

; For example, G-3-1 would be represented as...

(define AM-3-1
  (list
   (list 0 1 0)
   (list 1 0 0)
   (list 0 0 0)))

; meaning...
; - row 1 (representing vertex 1): can only reach vertex 2 (via column 2)
; - row 2 (representing vertex 2): can only reach vertex 1 (via column 1)
; - row 3 (representing vertex 3): can't reach any other vertex (since the row is all 0's)

; You have been supplied some tests for clarity.

; adjacency-matrix : Graph -> [List-of [List-of {0, 1}]]
; producing the adjacency-matrix representation of a graph

#|
(check-expect
 (adjacency-matrix G-2-1)
 (list
  (list 1 1)
  (list 0 0)))

(check-expect
 (adjacency-matrix G-3-1)
 AM-3-1)

(check-expect
 (adjacency-matrix G-5-2)
 (list
  (list 0 1 0 1 0)
  (list 0 0 0 0 1)
  (list 0 0 0 0 1)
  (list 0 0 0 0 0)
  (list 0 0 1 0 0)))
|#


; TODO #4: design the function am->graph that produces a graph given an adjacency matrix,
; as described in the previous todo. The vertices should be the numbers 1, 2, ... (length
; of the matrix). You have been supplied some tests for clarity.

; Hint: You will likely find the list-ref function quite useful.

; am->graph : [List-of [List-of {0, 1}]] -> Graph
; produces a graph given an adjacency matrix

#|
(check-expect
 (graph-v
  (am->graph AM-3-1))
 (list 1 2 3))

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  1 1)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  1 2)
 #true)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  1 3)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  2 1)
 #true)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  2 2)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  3 1)
 #false)

(check-expect
 ((graph-e
   (am->graph AM-3-1))
  3 2)
 #false)
|#



; TODO #5: design the function redirect that takes a graph and two vertices,
; and then produces a new graph that doesn't contain the first vertex and in which
; all edges whose destination was previously the first vertex should now be the second.
; You have been supplied some tests for clarity.

#|
(check-expect
 (graph-v (redirect G-2-2 3 7))
 (list 7))

(check-expect
 ((graph-e (redirect G-2-2 3 7)) 7 7)
 #true)

(check-expect
 (graph-v (redirect G-2-2 7 3))
 (list 3))

(check-expect
 ((graph-e (redirect G-2-2 7 3)) 3 3)
 #true)

(check-expect
 (graph-v (redirect G-5-2 3 4))
 (list 1 2 4 5))

(check-expect
 ((graph-e (redirect G-5-2 3 4)) 2 5)
 #true)

(check-expect
 ((graph-e (redirect G-5-2 3 4)) 5 4)
 #true)

(check-expect
 (graph-v (redirect G-5-2 4 3))
 (list 1 2 3 5))

(check-expect
 ((graph-e (redirect G-5-2 4 3)) 2 5)
 #true)

(check-expect
 ((graph-e (redirect G-5-2 4 3)) 1 3)
 #true)
|#


