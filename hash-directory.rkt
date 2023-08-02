#lang racket
(require racket/hash)
(require racket/future)
(provide pre-process)

;Input reader
(define (inputReader file)
  (define filetxt (port->lines(open-input-file file)))
  (close-input-port (current-input-port))
  filetxt
  )
; List of stop words.
(define stop (map string-upcase (inputReader "stop_words_english.txt")))

;Combine the input into one string.
(define (stringConcat textSofar textList)
  (if (empty? textList)
      textSofar
      (string-upcase (stringConcat (string-append (string-append textSofar (first textList)) " ") (rest textList)))
      )
  )

;Change anything that is not a letter, space, or ', and change it into a space.
(define (firstReplace letter)
  (if (not (or (or (char-alphabetic? letter) (char=? #\space letter)) (char=? #\' letter)))
      #\space
      letter
      )
  )

;Creates a list of words and filter out stop words. Keeps duplicates.
(define (makeWordList text)
 (filter-not (lambda (x) (member x stop)) (string-split (list->string (map firstReplace (string->list (stringConcat "" text))))))
  )

;Count word frequency
(define (createFrequencies wordList)
  (let ([wordCounts (map length (remove-duplicates (map (lambda (x) (indexes-of wordList x)) wordList)))])
     (map (lambda (x) (* -1 (log (/ x (foldl + 0 wordCounts)) 10)))wordCounts)
    )
  )

;New Hash function
(define (makeRelativeHash wordList)
  (let ([keys (remove-duplicates wordList)]
        [values (createFrequencies wordList)])
    (foldl hash-union #hash() (map hash keys values))
    )
  )

(define (pre-process dir)
  (define files (map (lambda (x) (string-append "Files/" x)) (map some-system-path->string (directory-list dir))))
    (define worker-thread (thread (lambda ()
                                  (let (
                                    [range1 (take files (round (/ (length files) 4)))]
                                    [range2 (take (drop files (round (/ (length files) 4))) (round (/ (length files) 4)))]
                                    [range3 (take (drop files (* (round (/ (length files) 4)) 2)) (round (/ (length files) 4)))]
                                    [range4 (drop files (* (round (/ (length files) 4)) 3))])
                                    (let ([worker0 (thread (lambda()  worker-thread (thread-send worker-thread  (map makeRelativeHash (map makeWordList (map inputReader range1))))))]
                                          [worker1 (thread (lambda()  worker-thread (thread-send worker-thread  (map makeRelativeHash (map makeWordList (map inputReader range2))))))]
                                          [worker2 (thread (lambda()  worker-thread (thread-send worker-thread  (map makeRelativeHash (map makeWordList (map inputReader range3))))))]
                                          [worker3 (thread (lambda()  worker-thread (thread-send worker-thread  (map makeRelativeHash (map makeWordList (map inputReader range4))))))]
                                          )
                                      (let ([keys (map some-system-path->string (directory-list dir))]
                                            [values (append (thread-receive) (thread-receive) (thread-receive) (thread-receive))]
                                            )
                                        (define output (foldl hash-union #hash() (map hash keys values)))
                                        (define outputfile (open-output-file "hash.txt" #:exists 'replace))
                                        (write output outputfile)
                                        (close-output-port outputfile)
                                        (displayln "Wrote to file")
                                        )
                                      )
                                  ))))
(thread-wait worker-thread)

  )

(pre-process "Files")
