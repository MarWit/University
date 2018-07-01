#lang racket

(struct fifo ((lst #:mutable)) #:transparent)

(define (make_queue)
    (fifo '())
)

(define (queue-empty? queue)
    (null? (fifo-lst queue))
)

(define (queue-add queue el)
    (set-fifo-lst! queue (mcons el (fifo (fifo-lst queue))))
)

(define (queue-pop queue)
    (match (fifo-lst queue)
        ['() (error 'empty_queue)]
        [(mcons a (fifo '())) (set-fifo-lst! queue '()) a]
        [(mcons a b) (queue-pop b)]
    )
)

(define (test)
    (let ([q (make_queue)])
        (displayln (queue-empty? q))
        (queue-add q 1)
        (queue-add q 2)
        (queue-add q 3)
        (displayln (queue-empty? q))
        (displayln (queue-pop q))
        (displayln (queue-pop q))
        (displayln (queue-pop q))
        (displayln (queue-empty? q))
    )
)
