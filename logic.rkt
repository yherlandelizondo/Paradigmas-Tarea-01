#lang racket
(require racket/base)
;(require "testLogic.rkt")

(define (mReverse list)
  (reverse-aux list '()))
(define (reverse-aux list newList)
  (cond((empty? list) newList)
       (else
        (reverse-aux (cdr list) (cons (car list) newList))
        )
       ))
;Testing
;(mReverse list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (values node pairs)
  (cond ((null? pairs) '())
        ((equal? node (caar pairs))(cons (caar pairs) (cdar pairs)))
        (else (values node (cdr pairs)))))
;Testing
;(values node pairs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (associations node list)
  (values node list))
;Testing
;(associations node list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Viene siendo el map
(define (applyFunction function list)
  (cond ((null? list)'())
        (else (cons (function (car list)) (applyFunction function (cdr list))))))
;Testing
;(display(ApplyFunction (lambda (x) (* x x)) '(1 2 3 4 5)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (member? element list)
  (cond((null? list)#f)
       ((equal? element (car list))#t)
       (else(member? element (cdr list)))))

(define (resolution? end route)
  (equal? end (car route)))
;Testing
;(resolution? end route)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (neighbors element graph); vecinos
  (cond ((equal? (associations element graph) #f)
         #f)
        (else(cadr (associations element graph)))))
;Testing
;(neighbors element graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extender ruta graph)
  (apply append
         (applyFunction (lambda(x)
                          (cond ((member? x ruta) '())
                                (else (list (cons x ruta)))))
                        (neighbors (car ruta) graph))))
;Testing
;(extender ruta graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (widthFirstAux rutas end graph total)
  (cond ((null? rutas)
         (applyFunction reverse total))
        ((resolution? end (car rutas))
         (widthFirstAux (cdr rutas) end graph (cons (car rutas) total)))
        ( else
          (widthFirstAux (append
                          (cdr rutas)(extender (car rutas) graph)) end graph total))))

(define (widthFirst first end graph) ;;Devuelve todas las rutas
  (widthFirstAux (list (list first)) end graph '()))
;Testing
;(widthFirst first end graph)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (lengthList list)
  (cond ((null? list)0)
        (else (+ 1 (lengthList (cdr list))))))
;Testing
;(lengthList list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (weight listWeights route)
  (cond ((equal? (lengthList route) 1)0)
        (else (+ (caadr (associations (list (car route) (cadr route)) listWeights)) (weight listWeights (cdr route))))))
;Testing
;(weight listWeights route)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compareWeight listWeights routes)
  (cond ((null? routes) 1000000)
        (else (min (weight listWeights (car routes)) (compareWeight listWeights (cdr routes))))))
;Testing
;(compareWeight listWeights routes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (findMinAux routes allRoutes listWeights)
  (cond((equal? (weight listWeights (car routes)) (compareWeight listWeights allRoutes)) (car routes))
       (else (findMinAux (cdr routes) allRoutes listWeights))))
(define(findMin routes listWeights);;ejemplo abajo
  (findMinAux routes routes listWeights))
;Testing
;(displayln (findMin (widthFirst 'a 'f graph) listWeights))
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
(define (graphCreator graph newNode)
  (cond((empty? newNode) graph)
       (else
        (reverse (cons (list newNode '()) (reverse graph) ))
        )
       ))
;Testing
;(graphCreator graph 'E)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (weightIndex start end weight wlist)
  (weightIndexAux start end weight wlist '())
  )

(define (weightIndexAux start end weight wlist blank)
  (reverse (cons (list (list start end) (cons weight blank)) (reverse wlist)))
  )
;Testing
;(weightIndex 'A 'D 45 listWeights)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pathCreator start end graph)
  (pathCreatorAux start end graph '())
  )
(define (pathCreatorAux start end tryList newGraph)
  (cond((empty? tryList) newGraph)
       ;;;;A == A
       ((equal? start (caar tryList)) (indexer start end tryList newGraph))
       (else ;; aplico cdr
        (pathCreatorAux start end (cdr tryList) (cons (car tryList) newGraph) )
        )
       ))
(define (indexer start end tryList newGraph)
  (cond((null? (cadar tryList))
        (pathCreatorAux start end (cdr tryList) (cons (list start (cons end (cadar tryList)) ) newGraph))
        )
       (else
        (pathCreatorAux start end (cdr tryList) (cons (list start (cons end (cadar tryList))) newGraph))
        )
       )
  )
;Testing
;(pathCreator 'A 'E graph)
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
;Testing
;(getValueWithIndex (getIndex 'Alajuela '(Cartago Heredia Alajuela Puntarenas Limon)) '((3, 8) c e k a))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (appendToList value listToAppend)
  (cons value listToAppend)
  )


(provide pathCreator weightIndex widthFirst graphCreator findMin appendToList mReverse)

