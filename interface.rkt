#lang racket
(require racket/gui/base)
(require "logic.rkt")

#|
    ////////////////////////////
    Main Window
    ////////////////////////////
|#
(define mainWindow (new frame% [label "Wazitico"]
                        [width 800]
                        [height 300]))

#|
    ////////////////////////////
    Window layout section
    ////////////////////////////
|#
(define mainPanel (new horizontal-panel% [parent mainWindow]))
(define leftPanel (new vertical-panel% [parent mainPanel]
                       [min-width 300]
                       [min-height 300]
                       ))

(define rightPanel (new vertical-panel% [parent mainPanel]
                        [min-width 500]
                        [min-height 300]
                        ))



#|
    ////////////////////////////
    graph construction section
    ////////////////////////////
|#

(define newNodeField (new text-field% [parent leftPanel]
                          [label "Agregar nodo"]))

(define buttonPanel (new horizontal-panel% [parent leftPanel]
                         [min-width 300]))

(define addNodeButton (new button% [parent buttonPanel]
                           [label "Agregar"]
                           [callback (lambda (button event)
                                       (newNodeButtonCallback event))]))

(define resetButton (new button% [parent buttonPanel]
                         [label "Reset"]
                         [callback (lambda (button event)
                                     (resetButtonCallback event))]))

(define originNode (new text-field% [parent leftPanel]
                        [label "Origen"]))

(define destinationNode (new text-field% [parent leftPanel]
                             [label "Destino"]))

(define pathWeight (new text-field% [parent leftPanel]
                        [label "Distancia"]))

(define bidirectionalCheckbox (new check-box% [parent leftPanel]
                                   [label "Bidireccional"]))

(define addEdgeButton (new button% [parent leftPanel]
                           [label "Agregar"]
                           [callback (lambda (button event)
                                       (newEdgeButtonCallback event))]))

(define searchMessage (new message% [parent leftPanel]
                           [label "Buscar ruta"]))

(define searchOrigin (new text-field% [parent leftPanel]
                          [label "Origen"]))

(define searchDestination (new text-field% [parent leftPanel]
                               [label "Destino"]))

(define routeSearchButton (new button% [parent leftPanel]
                               [label "Buscar"]))

#|
    Data collection section
|#

(define (newNodeButtonCallback event)
  (addNode (send newNodeField get-value)))



(define (newEdgeButtonCallback event)
  (addEdge (send originNode get-value) (send destinationNode get-value)
           (send pathWeight get-value) (send bidirectionalCheckbox get-value)))

(define (resetButtonCallback even)
  (reset))


#|
    canvas section
|#
(define canvas (new canvas% [parent rightPanel]))

#|
    show the frame
|#
(send mainWindow show #t)