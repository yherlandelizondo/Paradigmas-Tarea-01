#lang racket
(require racket/gui/base)
(require "logic.rkt")

#|
    ////////////////////////////
    Main Window
    ////////////////////////////
|#
(define mainWindow (new frame% [label "Wazitico"]
                        [width 1300]
                        [height 600]))

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
                           [label "Agregar"]
                           [callback (lambda (button event)
                                       (newNodeButtonCallback event))]))

(define resetButton (new button% [parent leftPanel]
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
    ////////////////////////////
    Data collection section
    ////////////////////////////
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
    ////////////////////////////
    Function to write data to a .txt file
    ////////////////////////////
|#
(define (writeFile graph edges path)
  #|
    ////////////////////////////
    creates the .txt file (if exist, replace it.)
    ////////////////////////////
|#

  (define output-port (open-output-file path #:exists 'replace))
  #|
    ////////////////////////////
    adding the file content
    ////////////////////////////
|#


  (write (list graph edges) output-port)

  #|
    close the file
  |#
  (close-output-port output-port))

#|
    ////////////////////////////
    Function for reading data from a file
    ////////////////////////////
|#
(define (readFile path)
  #|
    (open-input-file "./tmp/temp.txt") -> store the file descriptor (number to refer to an open file in the OS)
    (read (open-input-file "./tmp/temp.txt")) -> read the file, using the file descriptor
    (close-input-port (open-input-file "./tmp/temp.txt")) -> close the open file
  |#
  (define input-port (open-input-file path))
  (define file-content (read input-port))
  (close-input-port input-port)
  file-content)

#|
    ////////////////////////////
    Communication with the .txt file section
    ////////////////////////////
|#

(define (addNode node)
  (writeFile (graphCreator (car (readFile "./tmp/temp.txt")) (string->symbol node)) (cadr (readFile "./tmp/temp.txt")) "./tmp/temp.txt")
  (writeFile (appendToList (string->symbol node) (car (readFile "./tmp/temp2.txt"))) (cadr(readFile "./tmp/temp2.txt")) "./tmp/temp2.txt")

  )

(define (addEdge origin destination weight bid)

  #|
  bidirectional checkbox marked
|#
  (cond ((equal? bid #t) (writeFile (pathCreator (string->symbol origin) (string->symbol destination) (car (readFile "./tmp/temp.txt")))
                                    (weightIndex (string->symbol origin) (string->symbol destination) (string->number weight) (cadr (readFile "./tmp/temp.txt")))
                                    "./tmp/temp.txt"
                                    )

                         (writeFile (pathCreator (string->symbol destination) (string->symbol origin) (car (readFile "./tmp/temp.txt")))
                                    (weightIndex (string->symbol destination) (string->symbol origin) (string->number weight) (cadr (readFile "./tmp/temp.txt")))
                                    "./tmp/temp.txt"
                                    ))
        #|
  bidirectional checkbox not marked
|#
        (else (writeFile (pathCreator (string->symbol origin) (string->symbol destination) (car (readFile "./tmp/temp.txt")))
                         (weightIndex (string->symbol origin) (string->symbol destination) (string->number weight) (cadr (readFile "./tmp/temp.txt")))
                         "./tmp/temp.txt"
                         ))))

#|
    ////////////////////////////
    Reset and search for shortest parth section
    ////////////////////////////
|#
(define (reset)
  (writeFile '() '() "./tmp/temp.txt")
  (writeFile '() '((0 0) (5 5) (10 10) (25 56) (34 23) (45 45) (54 63) (100 100) (200 200) (110 110)) "./tmp/temp2.txt")
  )

(define (search origin destination)
  ;(writeFile (string->symbol origin) (string->symbol destination)))
  (writeFile (widthFirst (string->symbol origin) (string->symbol destination) (car (readFile "./tmp/temp.txt")))
             (findMin (widthFirst (string->symbol origin) (string->symbol destination) (car (readFile "./tmp/temp.txt"))) (cadr (readFile "./tmp/temp.txt")))
             "./tmp/temp.txt"
             )
  )
#|
    ////////////////////////////
    canvas section
    ////////////////////////////
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define condition #f)

(define graphicButton (new button% [parent leftPanel]
                           [label "Graficar"]
                           [callback (lambda (button event)
                                       (set-condition (not (get-condition)))
                                       (send canvas refresh))]))
(define (paint-callback canvas dc)
  (when condition
    (nodePainter (mReverse (car (readFile "./tmp/temp2.txt"))) dc 0)
    ;(shapeNode dc 10 10 "nacho")
    )
  )

(define (nodePainter nodeList dc index)
  (cond ((equal? nodeList null) nodeList)
        (else

         (shapeNode dc  (car (getValueWithIndex (+ index (getIndex (car nodeList) nodeList)) (cadr (readFile "./tmp/temp2.txt"))))
                    (cadr (getValueWithIndex (+ index (getIndex (car nodeList) nodeList)) (cadr (readFile "./tmp/temp2.txt"))))
                    (symbol->string (car nodeList)))
         (nodePainter (cdr nodeList) dc (+ index 1))
         )
        )
  )

;(getValueWithIndex 0 (cadr (readFile "./tmp/temp2.txt")))

(car (getValueWithIndex (getIndex (car (mReverse (car (readFile "./tmp/temp2.txt")))) (mReverse (car (readFile "./tmp/temp2.txt")))) (cadr (readFile "./tmp/temp2.txt"))))

(define canvas (new canvas% [parent rightPanel]
                    [style (list 'border)]
                    [paint-callback paint-callback]
                    ))

(define (set-condition state)
  (set! condition state))

(define (get-condition)
  condition)

;METHOD TO DRAW AN INDIVIDUAL NODE
(define (shapeNode dc xPos yPos nodeName)
  (send dc set-scale 3 3)
  (send dc set-font (make-font #:size 3))
  (send dc set-text-foreground "black")
  (send dc draw-ellipse xPos yPos 25 25)
  (send dc draw-text nodeName (+ xPos 1) (+ yPos 9) )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
    ////////////////////////////
    show the frame
    ////////////////////////////
|#
(send mainWindow show #t)
