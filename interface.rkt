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

(define searchButton (new button% [parent leftPanel]
                          [label "Buscar"]
                          [callback (lambda (button event)
                                      (searchButtonCallback event))]))

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

(define (searchButtonCallback event)
  ;(addNode (send searchDestination get-value)))
  (search (send searchOrigin get-value) (send searchDestination get-value)))


#|
    ////////////////////////////
    Read and write section
    ////////////////////////////
|#
#|
  Function to write data to a .txt file
|#
(define (writeFile graph edges)
  #|
    create the .txt file; if exist, replace it.
  |#
  (define output-port (open-output-file "./tmp/temp.txt" #:exists 'replace))

  #|
    adding the file content
  |#
  (write (list graph edges) output-port)

  #|
    close the file
  |#
  (close-output-port output-port))

#|
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

#|
  Function for reading data from a file
|#
(define (readFile)
  #|
    (open-input-file "./tmp/temp.txt") -> store the file descriptor (number to refer to an open file in the OS)
    (read (open-input-file "./tmp/temp.txt")) -> read the file, using the file descriptor
    (close-input-port (open-input-file "./tmp/temp.txt")) -> close the open file
  |#
  (define input-port (open-input-file "./tmp/temp.txt"))
  (define file-content (read input-port))
  (close-input-port input-port)
  file-content)

#|
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(define (addNode node)
  (writeFile (graphCreator (car (readFile)) (string->symbol node)) (cadr (readFile))))

#|
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(define (addEdge origin destination weight bid)

  #|
  bidirectional checkbox marked
|#
  (cond ((equal? bid #t) (writeFile (pathCreator (string->symbol origin) (string->symbol destination) (car (readFile)))
                                    (weightIndex (string->symbol origin) (string->symbol destination) (string->number weight) (cadr (readFile))))

                         (writeFile (pathCreator (string->symbol destination) (string->symbol origin) (car (readFile)))
                                    (weightIndex (string->symbol destination) (string->symbol origin) (string->number weight) (cadr (readFile)))))
        #|
  bidirectional checkbox not marked
|#
        (else (writeFile (pathCreator (string->symbol origin) (string->symbol destination) (car (readFile)))
                         (weightIndex (string->symbol origin) (string->symbol destination) (string->number weight) (cadr (readFile)))))))
#|
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(define (reset)
  (writeFile '() '()))

(define (search origin destination)
  ;(writeFile (string->symbol origin) (string->symbol destination)))
  (writeFile (widthFirst (string->symbol origin) (string->symbol destination) (car (readFile)))
             (findMin (widthFirst (string->symbol origin) (string->symbol destination) (car (readFile))) (cadr (readFile)))
             )
  )
;(widthFirst (string->symbol origin) (string->symbol destination) (car (readFile)))
;(findMin (widthFirst (string->symbol origin) (string->symbol destination) (car (readFile))) (cdr (readFile)))
#|
    canvas section
|#
(define canvas (new canvas% [parent rightPanel]))

#|
    show the frame
|#
(send mainWindow show #t)
