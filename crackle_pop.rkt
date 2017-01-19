#lang racket

(map crackle-pop (enumerate-interval 0 100))

; enumerate interval - makes a list from 0 to 100
(define (enumerate-interval low high)
  (if (> low high)
    null
    (cons low
      (enumerate-interval (+ low 1) high))))

; crackle-pop? as a conditional procedure
(define (crackle-pop n)
  (cond ((and (= (remainder n 3) 0) 
              (= (remainder n 5) 0))
              (quote 'crackle-pop))
        ((= (remainder n 3) 0)
          (quote 'crackle))
        ((= (remainder n 5) 0)
          (quote 'pop))
      (else n))
  )

; map - applies a procedure to a sequence, returns a list
(define (map proc sequence)
  (if (null? sequence)
        null
        (cons (proc (car sequence))
              (map proc (cdr sequence)))))