#lang racket
;(require "hash-directory.rkt")
(require data/maybe)
(require data/monad)
(define (input)
  (define input (open-input-file "hash.txt"))
  (define filetxt (read/recursive input))
  (close-input-port input)
  (if (eof-object? filetxt) nothing (if (not (hash? filetxt)) nothing (just filetxt)))
  )



(define (findWord term)
  (let ([currH (from-just #f (from-just #f (just (input))))])
  
  (if (hash? currH)
      (hash-map currH (lambda (key x) (if (hash-has-key? x (string-upcase term)) (hash-ref x (string-upcase term)) 0 )))
      nothing
      )
    ))


(define (rankSingle input)
  (sort (map (lambda (x y) (cons (list x) y)) input (map findWord input)) (lambda (x y) (< (length(indexes-of x 0)) (length(indexes-of y 0)))))

  )

(define (rankGroup input)
  (range (length (first (map findWord input))))
  (map (lambda (x y) (list-ref x y)) (map findWord input) (range (length (first (map findWord input)))))
  )

(define (main)
  (display "Input: ")
  (let ([x (read-line)])
     (rankSingle (string-split x " "))
))
(main)