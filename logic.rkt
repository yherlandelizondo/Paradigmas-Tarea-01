#lang racket
(require racket/base)
;*************************************************************************************************
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



;(displayln (findMin (widthFirst 'a 'f graph) listWeights))


#|
  ///////////////////////////////parte nacho///////////////////////////////////////
|#



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

(define (lastElement list)
  (lastElement-aux list '()))
(define (lastElement-aux list lastOne)
  (cond((empty? list) lastOne)
       ((lastElement-aux (cdr list) (car list)))
       ))
;Testing
;(lastElement '(A))
;//////////////////////////////////////// S E C O N D  C O M M I T ///////////////////////////////////////////////

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
  (reverse (cons (list (list start end) weight) (reverse wlist)))
  )
;Testing
;(weightIndex 'A 'D 45 wlist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns a list with the order reversed
(define (Mreverse list)
  (reverse-aux list '()))
(define (reverse-aux list newList)
  (cond((empty? list) newList)
       (else
        (reverse-aux (cdr list) (cons (car list) newList))
        )
       ))

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

#|
  ////////////////////////////////////////////////////////////////////////////////
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

(provide addNode reset addEdge)

