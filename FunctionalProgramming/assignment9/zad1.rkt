#lang racket

(define (count-change amount coins)
    (cond
      [(null? coins) 0]
      [(< amount 0) 0]
      [(= amount 0) 1]
      [else
        (+
            (count-change amount (cdr coins))
            (count-change (- amount (car coins)) coins)
        )]
    )
)

(count-change 100 '(1 5 10 25 50))
