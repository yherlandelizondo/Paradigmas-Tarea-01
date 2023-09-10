#lang racket

;Testing lists
(define graph  '( (a (e))
                  (e (a b g))
                  (b (e g f d))
                  (c (d))
                  (d (c b))
                  (g (b e))
                  (f (b))
                  ))

(define listWeights '(((a e) (3))
                      ((e a) (3))
                      ((e g) (8))
                      ((g e) (8))
                      ((e b) (1))
                      ((b e) (1))
                      ((b g) (7))
                      ((g b) (7))
                      ((b f) (9))
                      ((f b) (9))
                      ((d b) (4))
                      ((b d) (4))
                      ((c d) (5))
                      ((d c) (5))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
  testing

(writeFile '((a (a b c)) (a (e f g)) (b (1 2 3)) (c ()))
           '((Hola mundo mi nombre es hola))
           )

(display (readFile))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list with the order reversed
(define (Mreverse list)
  (reverse-aux list '()))
(define (reverse-aux list newList)
  (cond((empty? list) newList)
       (else
        (reverse-aux (cdr list) (cons (car list) newList))
        )
       ))

(define (lastElement list)
  (lastElement-aux list '()))
(define (lastElement-aux list lastOne)
  (cond((empty? list) lastOne)
       ((lastElement-aux (cdr list) (car list)))
       ))
;Testing
;(lastElement '(A))
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
;Associates an element with a pair list, returns only the missing part
(define (twin key list)
  (cond ((empty? list) #f)
        ((equal? (car (car list)) key) (cdar list))
        (else (twin key (cdr list)))
        ))
;Testing
;(connection 'B graph)
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

(provide listWeights graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
