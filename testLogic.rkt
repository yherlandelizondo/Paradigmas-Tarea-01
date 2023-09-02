#lang racket

(define (displayNewNode node)
  (displayln (format "Nodo recibido: ~a" node)))


(define (displayEdge firstNode secondNode weight bid)
  (displayln (format "Nodo A: ~a Nodo B: ~a Peso: ~a Bidireccional: ~a" firstNode secondNode weight bid)))

(provide displayNewNode displayEdge)

