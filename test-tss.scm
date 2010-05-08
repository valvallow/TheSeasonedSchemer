(load "./20.whatsinstore.scm")

(value '(sub1 1))
; -> 0

(value '(add1 1))
; -> 2

(value '(define x 3))
(value 'x)
; -> 3

(value '(set! x 5))
(value 'x)
; -> 5

(value '(lambda (x) x))

(value '((lambda (y)
           (set! x 7) y) 0))

(value 'x)
; -> 7

(value '(define odd?
          (lambda (n)
            (cond
             ((zero? n) #f)
             (else (even? (sub1 n)))))))

(value '(define even?
          (lambda (n)
            (cond
             ((zero? n) #t)
             (else (odd? (sub1 n)))))))

(value 'odd?)

(value '(odd? 0))
; -> #f

(value '(odd? 5))
; -> #t

(value '(even? 1))
; -> #f

(value '(even? 100))
; -> #t


