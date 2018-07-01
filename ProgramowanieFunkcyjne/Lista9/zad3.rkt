#lang racket

(define (simplify func)
    (match func
        [`(+ 0 0) 0]
        [`(+ 0 ,rhs) (simplify rhs)]
        [`(+ ,lhs 0) (simplify lhs)]
        [`(+ ,lhs ,rhs) `(+ ,(simplify lhs) ,(simplify rhs))]
        [`(* 0 0) 0]
        [`(* 1 1) 0]
        [`(* ,lhs 0) 0]
        [`(* 0 ,rhs) 0]
        [`(* ,lhs 1) (simplify lhs)]
        [`(* 1 ,rhs) (simplify rhs)]
        [num #:when (number? num) num]
        [atom atom]
    )
)

(define (deriv func var)
    (match func
        [`(+ ,lhs ,rhs)
            (define clhs (simplify (deriv lhs var)))
            (define crhs (simplify (deriv rhs var)))
            (simplify `(+ ,clhs ,crhs))
        ]
        [`(* ,lhs ,rhs)
            (define slhs (simplify lhs))
            (define srhs (simplify rhs))
            (define clhs (simplify (deriv slhs var)))
            (define crhs (simplify (deriv srhs var)))
            (simplify `(+
                ,(simplify `(* ,clhs ,rhs))
                ,(simplify `(* ,lhs ,crhs))
            ))
        ]
        [atom #:when (equal? atom var) 1]
        [_ 0]
    )
)

(define (test-deriv1)
    (deriv '(* x y) 'x)
)

(define (test-deriv2)
    (deriv '(* (* x y) (+ x 3)) 'x)
)
