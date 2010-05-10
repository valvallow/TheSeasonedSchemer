;; The Seasoned Shcmer
;; chapter 20

(load "./20.whatsinstore.scm")

(value '(sub1 1))
; -> 0

(value '(add1 1))
; -> 2

(value '(def x 3))
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

(value '(def odd?
          (lambda (n)
            (cond
             ((zero? n) #f)
             (else (even? (sub1 n)))))))

(value '(def even?
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



(define-syntax tss-lisp-eval
  (syntax-rules ()
    ((_ expr)
     (value (quote expr)))))

(define-syntax multi-tss-lisp-eval
  (syntax-rules ()
    ((_ expr)
     (tss-lisp-eval expr))
    ((_ expr1 expr2 ...)
     (begin (tss-lisp-eval expr1)
            (multi-tss-lisp-eval expr2 ...)))))

(define my-lisp multi-tss-lisp-eval)



(my-lisp
 (def &+
   (lambda (n m)
     (cond ((zero? m) n)
           (else (&+ (add1 n)(sub1 m))))))
 (def &-
   (lambda (n m)
     (cond ((zero? m) n)
           (else (&- (sub1 n)(sub1 m))))))
 (def &*
   (lambda (n m)
     (cond ((zero? m) 0)
           (else (&+ n (&* n (sub1 m)))))))
 (def &>
   (lambda (n m)
     (cond ((zero? n) #f)
           ((zero? m) #t)
           (else (&> (sub1 n)(sub1 m))))))
 (def &<
   (lambda  (n m)
     (cond ((zero? m) #f)
           ((zero? n) #t)
           (else (&< (sub1 n)(sub1 m))))))
 (def &=
   (lambda (n m)
     (cond ((&> n m) #f)
           ((&< n m) #f)
           (else #t))))
 (def &^
   (lambda(n m)
     (cond ((zero? m) 1)
           (else (&* n (&^ n (sub1 m)))))))
 (def &/
   (lambda (n m)
     (cond ((&< n m) 0)
           (else (add1 (&/ (&- n m) m)))))))


(my-lisp (&* 5 6))
; -> 30

(my-lisp (&/ 25 5))
; -> 5

(my-lisp
 (def fact
      (lambda (n)
        (cond
         ((zero? n) 1)
         (else (&* n (fact (&- n 1)))))))
 (fact 5))
; -> 120


