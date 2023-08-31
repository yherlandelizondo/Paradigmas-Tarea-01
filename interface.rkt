#lang racket
(require racket/gui/base)

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

(define addNodeButton (new button% [parent leftPanel]
                           [label "Agregar"]))

(define originNode (new text-field% [parent leftPanel]
                        [label "Origen"]))

(define destinationNode (new text-field% [parent leftPanel]
                             [label "Destino"]))

(define bidirectionalCheckbox (new check-box% [parent leftPanel]
                                   [label "Bidireccional"]))

(define addEdgeButton (new button% [parent leftPanel]
                           [label "Agregar"]))

(define searchMessage (new message% [parent leftPanel]
                           [label "Buscar ruta"]))

(define searchOrigin (new text-field% [parent leftPanel]
                          [label "Origen"]))

(define searchDestination (new text-field% [parent leftPanel]
                               [label "Destino"]))

(define routeSearchButton (new button% [parent leftPanel]
                               [label "Buscar"]))

#|
    canvas section
|#
(define canvas (new canvas% [parent rightPanel]))

#|
    show the frame
|#
(send mainWindow show #t)