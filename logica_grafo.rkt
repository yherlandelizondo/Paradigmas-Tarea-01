#lang racket
;Testing graph
(define graph '(
                (A (B C E F))
                (B (C D))
                (C (D E F))
                (D ())
                ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The node exist in the graph?
(define (exist node graph)
  (cond((null? graph) #f)
       ((equal? node (caar graph))#t)
       (else
        (exist node (cdr graph))
        )))
;Testing
;(exist 'C graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns a list with the order reversed
(define (reverse list)
  (reverse-aux list '()))
(define (reverse-aux list newList)
  (cond((empty? list) newList)
       (else
        (reverse-aux (cdr list) (cons (car list) newList))
        )
       ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Turn a graph into a list
(define (listMake graph)
  (listMake-aux graph '()))
(define (listMake-aux graph listReady)
  (cond((empty? graph) (reverse listReady))
       (else
        (listMake-aux (cdr graph) (cons (caar graph) listReady))
        )
       ))
;Testing
;(listMake graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Associates an element with a pair list, returns only the missing part
(define (twin key list)
  (cond ((empty? list) #f)
        ((equal? (car (car list)) key) (cdar list))
        (else (twin key (cdr list)))
        ))
;Testing
;(connection 'B graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Shows the available paths from a specific point
(define (paths start graph)
  (FinalPath (car (twin start graph)) start '() ))

(define (paths-aux connection start pseudoPath)
  (cond((empty? connection) (reverse pseudoPath))
       (else (paths-aux '() start  (cons (car connection) pseudoPath) ))
       ))

(define (FinalPath connections start roads )
  (cond((empty? connections) (reverse roads))
       (else
        (FinalPath (cdr connections) start (cons (paths-aux connections start (cons start '())) roads) )
        )))
;Testing
;(paths 'A graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lastElement list)
  (lastElement-aux list '()))
(define (lastElement-aux list lastOne)
  (cond((empty? list) lastOne)
       ((lastElement-aux (cdr list) (car list)))
       ))
;Testing
;(lastElement '(A))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pathFinder start graph)
  (pathfinder-aux start graph (paths (lastElement start) graph) '() '() )
  )
(define (pathfinder-aux start graph linkZone newElement linkDone)
  (cond((empty? linkZone) (reverse linkDone))
       (else
        (pathfinder-aux start graph (cdr linkZone) newElement
                        (cons (reverse (cons (lastElement (car linkZone)) (reverse start) )) linkDone) )
        )))
;Testing
(pathFinder '(A B C) graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

