#lang racket
(require racket/base)

;return the reverse of a list
(define (mReverse list)
  (reverse-aux list '()))
(define (reverse-aux list newList)
  (cond((empty? list) newList)
       (else
        (reverse-aux (cdr list) (cons (car list) newList))
        )
       ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The value function is used to look up a specific node in a list of pairs
;and return the list of associations that include that node.

(define (associations node pairs)
  (cond ((null? pairs) '())
        ((equal? node (caar pairs))(cons (caar pairs) (cdar pairs)))
        (else (associations node (cdr pairs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The value function is used to look up a specific node in a list of pairs
;and return the list of associations that include that node.

#|
(define (associations node list)
  (values node list))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The function is used to apply a given function to each item in a list and
;returns a new list containing the results of applying the function to each item.

(define (applyFunction function list)
  (cond ((null? list)'())
        (else (cons (function (car list)) (applyFunction function (cdr list))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The function indicates whether or not an element belongs to a list.

(define (member? element list)
  (cond((null? list)#f)
       ((equal? element (car list))#t)
       (else(member? element (cdr list)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function that determines if the desired solution has been reached.

(define (resolution? end route)
  (equal? end (car route)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function that returns the immediate neighbors of a node.
(define (neighbors element graph); vecinos
  (cond ((equal? (associations element graph) #f) ;non-existent node
         #f)
        (else(cadr (associations element graph)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function that extends a partial path with possible alternative routes.
(define (extend path graph)
  (apply append
         (applyFunction (lambda(x) ;Checks if the neighbors of the first element in the path belong to the path.
                          (cond ((member? x path) '()) ;exist
                                (else (list (cons x path))))) ;doesn't exist
                        (neighbors (car path) graph))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Auxiliary function to extend the partial path.
(define (widthFirstAux paths end graph total)
  (cond ((null? paths)
         (applyFunction reverse total))
        ((resolution? end (car paths)) ;checks if the first element of paths is the destination
         (widthFirstAux (cdr paths) end graph (cons (car paths) total))) ;recursive call with the first element added to the total list
        ( else
          (widthFirstAux (append
                          (cdr paths)(extend (car paths) graph)) end graph total)))) ; recursive call with the first element removed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Main function for breadth-first traversal and search of routes
; Returns the list with all found routes in (recorridoAux)
(define (widthFirst first end graph)
  (widthFirstAux (list (list first)) end graph '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns the length of a list
(define (lengthList list)
  (cond ((null? list)0)
        (else (+ 1 (lengthList (cdr list))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Calculate the weight of a route on a graph
(define (weight listWeights route)
  (cond ((equal? (lengthList route) 1)0)
        (else (+ (caadr (associations (list (car route) (cadr route)) listWeights)) (weight listWeights (cdr route))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This function compares all the weights of all possible paths from one point to
;another on a graph and returns the one with the lowest weight.
(define (compareWeight listWeights routes)
  (cond ((null? routes) exp 10 6)
        (else (min (weight listWeights (car routes)) (compareWeight listWeights (cdr routes))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (findMinAux routes allRoutes listWeights)
  (cond((equal? (weight listWeights (car routes)) (compareWeight listWeights allRoutes)) (car routes))
       (else (findMinAux (cdr routes) allRoutes listWeights))))

;function to search the shortest path
(define(findMin routes listWeights)
  (findMinAux routes routes listWeights))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Add a Node to a graph
(define (graphCreator graph newNode)
  (cond((empty? newNode) graph)
       (else
        (reverse (cons (list newNode '()) (reverse graph) ))
        )
       ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Input:The origin and the end of a conection between nodes and its weight
;Output: the list of all the weights and its connections with the new connection indexed

(define (weightIndex start end weight wlist)
  (weightIndexAux start end weight wlist '())
  )

(define (weightIndexAux start end weight wlist blank)
  (reverse (cons (list (list start end) (cons weight blank)) (reverse wlist)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Input: A node, the node that is connected to the first one, their graph
;Output: the graph with the new connection between nodes

(define (pathCreator start end graph)
  (pathCreatorAux start end graph '());cast to the auxiliar function
  )
(define (pathCreatorAux start end tryList newGraph)
  (cond((empty? tryList) newGraph)
       ;;;;A == A
       ((equal? start (caar tryList)) (indexer start end tryList newGraph))
       (else ;; apply cdr
        (pathCreatorAux start end (cdr tryList) (cons (car tryList) newGraph) )
        )
       ))
(define (indexer start end tryList newGraph)
  (cond((null? (cadar tryList));checks if the nodes has other connections
        (pathCreatorAux start end (cdr tryList) (cons (list start (cons end (cadar tryList)) ) newGraph))
        )
       (else ;the node has other connections
        (pathCreatorAux start end (cdr tryList) (cons (list start (cons end (cadar tryList))) newGraph))
        )
       )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Return the index of a specific member of the list

(define (getIndexAux value list cont)
  (cond((equal? (car list) value) cont)
       (else
        (getIndexAux value (cdr list) (+ cont 1)))
       ))

(define (getIndex value list)
  (getIndexAux value list 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;return the index value in the list

(define (getValueWithIndexAux index list cont)
  (cond((equal? cont index) (car list))
       (else
        (getValueWithIndexAux index (cdr list) (+ cont 1)))
       ))

(define (getValueWithIndex index list)
  (getValueWithIndexAux index list 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;funtion to append an element to the list

(define (appendToList value listToAppend)
  (cons value listToAppend)
  )

(provide pathCreator weightIndex widthFirst graphCreator findMin appendToList mReverse getValueWithIndex getIndex weight)

