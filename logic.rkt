#lang racket
;*************************************************************************************************
(define (val node pairs)
    (cond ((null? pairs) '())
    ((equal? node (caar pairs))(cons (caar pairs) (cdar pairs)))
    (else (val node (cdr pairs)))))
(define (Associations node list)
    (val node list))
;*************************************************************************************************
(define grafo  '( (a (e))
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
(define (miembro? elem list)
  (cond((null? list)#f)
       ((equal? elem (car list))#t)
       (else(miembro? elem (cdr list)))))

(define (solucion? fin ruta)
  (equal? fin (car ruta)))

(define (vecinos ele grafo)
    (cond ((equal? (Associations ele grafo) #f)
           #f)
          (else(cadr (Associations ele grafo)))))

(define (extender ruta grafo)
  (apply append
         (map (lambda(x)
                    (cond ((miembro? x ruta) '())
                          (else (list (cons x ruta)))))
              (vecinos (car ruta) grafo))))

(define (anchura-todas-aux rutas fin grafo total)
  (cond ((null? rutas)
         (map reverse total))
        ((solucion? fin (car rutas))
         (anchura-todas-aux (cdr rutas) fin grafo (cons (car rutas) total)))
        ( else
          (anchura-todas-aux (append
           (cdr rutas)(extender (car rutas) grafo)) fin grafo total))))

(define (anchura-todas ini fin grafo)
  (anchura-todas-aux (list (list ini)) fin grafo '()))
;*************************************************************************************************
(define (longitud list)
  (cond ((null? list)0)
  (else (+ 1 (longitud (cdr list))))))
;*************************************************************************************************
(define (Weight listWeights route)
  (cond ((equal? (longitud route) 1)0)
    (else (+ (caadr (Associations (list (car route) (cadr route)) listWeights)) (Weight listWeights (cdr route))))))
;*************************************************************************************************
(define (CompareWeight listWeights routes)
  (cond ((null? routes) 1000000)
  (else (min (Weight listWeights (car routes)) (CompareWeight listWeights (cdr routes))))))
;*************************************************************************************************
(define (FindMin routes allRoutes listWeights)
  (cond((equal? (Weight listWeights (car routes)) (CompareWeight listWeights allRoutes)) (car routes))
    (else (FindMin (cdr routes) allRoutes listWeights))))

(displayln (FindMin (anchura-todas 'a 'b grafo) (anchura-todas 'a 'b grafo) listWeights))