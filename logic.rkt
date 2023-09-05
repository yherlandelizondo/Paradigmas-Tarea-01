#lang racket
;*************************************************************************************************
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
;*************************************************************************************************
(define (values node pairs)
    (cond ((null? pairs) '())
    ((equal? node (caar pairs))(cons (caar pairs) (cdar pairs)))
    (else (values node (cdr pairs)))))
(define (associations node list)
    (values node list))
;************************************************************************************************
;Viene siendo el map
(define (applyFunction function list)
  (cond ((null? list)'())
  (else (cons (function (car list)) (applyFunction function (cdr list))))))
;(display(ApplyFunction (lambda (x) (* x x)) '(1 2 3 4 5)))

;*************************************************************************************************
(define (member? element list)
  (cond((null? list)#f)
       ((equal? element (car list))#t)
       (else(member? element (cdr list)))))

(define (resolution? end route)
  (equal? end (car route)))

(define (neighbors element graph); vecinos
    (cond ((equal? (associations element graph) #f)
           #f)
          (else(cadr (associations element graph)))))

(define (extender ruta graph)
  (apply append
         (applyFunction (lambda(x)
                    (cond ((member? x ruta) '())
                          (else (list (cons x ruta)))))
              (neighbors (car ruta) graph))))

(define (widthFirstAux rutas end graph total)
  (cond ((null? rutas)
         (applyFunction reverse total))
        ((resolution? end (car rutas))
         (widthFirstAux (cdr rutas) end graph (cons (car rutas) total)))
        ( else
          (widthFirstAux (append
           (cdr rutas)(extender (car rutas) graph)) end graph total))))

(define (widthFirst first end graph)
  (widthFirstAux (list (list first)) end graph '()))
;*************************************************************************************************
(define (lengthList list)
  (cond ((null? list)0)
  (else (+ 1 (lengthList (cdr list))))))
;*************************************************************************************************
(define (weight listWeights route)
  (cond ((equal? (lengthList route) 1)0)
    (else (+ (caadr (associations (list (car route) (cadr route)) listWeights)) (weight listWeights (cdr route))))))
;*************************************************************************************************
(define (compareWeight listWeights routes)
  (cond ((null? routes) 1000000)
  (else (min (weight listWeights (car routes)) (compareWeight listWeights (cdr routes))))))
;*************************************************************************************************
(define (findMinAux routes allRoutes listWeights)
  (cond((equal? (weight listWeights (car routes)) (compareWeight listWeights allRoutes)) (car routes))
    (else (findMinAux (cdr routes) allRoutes listWeights))))
(define(findMin routes listWeights)
(findMinAux routes routes listWeights))



(displayln (findMin (widthFirst 'a 'f graph) listWeights))
