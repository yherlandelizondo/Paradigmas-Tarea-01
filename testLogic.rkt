#lang racket

(define (displayNewNode node)
  (displayln (format "Nodo recibido: ~a" node)))


(define (displayEdge firstNode secondNode weight bid)
  (displayln (format "Nodo A: ~a Nodo B: ~a Peso: ~a Bidireccional: ~a" firstNode secondNode weight bid)))

(provide displayNewNode displayEdge)

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
  ////////////////////////////////////////////////////////////////////////////////
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
  (close-input-port (open-input-file "./tmp/temp.txt")) (read (open-input-file "./tmp/temp.txt")))

#|
  ////////////////////////////////////////////////////////////////////////////////
|#

#|
  testing
|#
(writeFile '((a (a b c)) (a (e f g)) (b (1 2 3)) (c ()))
           '((Hola mundo mi nombre es hola))
           )

(display (readFile))