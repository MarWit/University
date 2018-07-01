#lang racket

(define (mk-mobile left right) (cons left right))
(define (mk-branch length struct) (cons length struct))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))
(define (branch-struct branch) (cdr branch))

(define (weight? branch) (number? branch))

(define (calc-weight mobile)
    (if (null? mobile) 0
        (if (weight? mobile) mobile
            (let ([left (left-branch mobile)] [right (right-branch mobile)])
                (+
                    (calc-weight (branch-struct left))
                    (calc-weight (branch-struct right))
                )
            )
        )
    )
)

(define (balanced? mobile)
    (if (or (null? mobile) (weight? mobile)) #t
        (let ([left (left-branch mobile)] [right (right-branch mobile)])
            (and (equal? (* (calc-weight (branch-struct left)) (branch-length left))
                         (* (calc-weight (branch-struct right)) (branch-length right)))
                 (and (balanced? (branch-struct left)) (balanced? (branch-struct right)))
            )
        )
    )
)

(define (struc)
    (mk-mobile
        (mk-branch 3
            (mk-mobile
                (mk-branch 4 7)
                (mk-branch 2 8)
            )
        )
        (mk-branch 3
            (mk-mobile
                (mk-branch 2 8)
                (mk-branch 4 7)
            )
        )
    )
)

(define (struc2)
    (mk-mobile
        (mk-branch 3
            (mk-mobile
                (mk-branch 16 2)
                (mk-branch 4 8)
            )
        )
        (mk-branch 3
            (mk-mobile
                (mk-branch 16 2)
                (mk-branch 4 8)
            )
        )
    )
)
