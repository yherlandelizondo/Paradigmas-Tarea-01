#lang racket

(define (displayNewNode node)
  (displayln (format "Nodo recibido: ~a" node)))


(define (displayEdge firstNode secondNode weight bid)
  (displayln (format "Nodo A: ~a Nodo B: ~a Peso: ~a Bidireccional: ~a" firstNode secondNode weight bid)))

(provide displayNewNode displayEdge)


#|
  testing

(writeFile '((a (a b c)) (a (e f g)) (b (1 2 3)) (c ()))
           '((Hola mundo mi nombre es hola))
           )

(display (readFile))
|#