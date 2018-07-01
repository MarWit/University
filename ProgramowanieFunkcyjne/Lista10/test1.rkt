
(define (test a b c) (+ a b c ))
(define (t1) (print "A") 1)
(define (t2) (print "B") 2)
(define (t3) (print "C") "xD")

(test (t1) (t2) (t3))
