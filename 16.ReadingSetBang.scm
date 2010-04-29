;; chapter.16

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(define last (quote angelfood))

(sweet-tooth (quote chocolate))
; -> (chocolate cake)

last
; -> angelfood

(sweet-tooth (quote fruit))
; -> (fruit cake)

last
; -> angelfood


(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(sweet-toothL (quote chocolate))
; -> (chocolate cake)

last
; -> chocolate

(sweet-toothL (quote fruit))
; -> (fruit cake)

last
; -> fruit

(sweet-toothL (quote cheese))
; -> (cheese cake)

(sweet-toothL (quote carrot))
; -> (carrot cake)


(define ingredients (quote ()))

(define sweet-toothR
  (lambda (food)
    (set! ingredients
          (cons food ingredients))
    (cons food
          (cons (quote cake)
                (quote ())))))

(sweet-toothR (quote chocolate))
; -> (chocolate cake)

ingredients
; -> (chocolate)

(sweet-toothR (quote fruit))
; -> (fruit cake)

ingredients
; -> (fruit chocolate)

(sweet-toothR (quote cheese))
; -> (cheese cake)

ingredients
; -> (cheese fruit chocolate)

(sweet-toothR (quote cheese))
; -> (cheese cake)

ingredients
; -> (cheese cheese fruit chocolate)


;; deep

;; (deep 3)
;; -> (((pizza)))

;; (deep 7)
;; -> (((((((pizza)))))))

;; (deep 0)
;; -> pizza


(define (deep n)
  (if (zero? n)
      'pizza
      (cons (deep (- n 1))
            '())))

(deep 3)
; -> (((pizza)))


;; again letrec
(define (make-deep a)
  (lambda (n)
    (letrec
        ((d (lambda (n)
              (if (zero? n)
                  a
                  (cons (d (- n 1)) '())))))
      (d n))))

((make-deep 'pizza) 3)
; -> (((pizza)))


;; again named-let, accumulate
(define (make-deep a)
  (lambda (n)
    (let loop ((n n)
               (acc a))
      (if (zero? n)
          acc
          (loop (- n 1)
                (cons acc '()))))))

((make-deep 'pizza) 3)
; -> (((pizza)))


;; again fold-right
(use srfi-1)
(define (make-deep a)
  (lambda (n)
    (fold-right (lambda (e acc)
                  (if (zero? e)
                      acc
                      (cons acc '())))
                a
                (iota (+ n 1)))))

((make-deep 'pizza) 5)
; -> (((((pizza)))))


;; The Seasoned Schemer
(define sub1
  (lambda (n)
    (- n 1)))

(define deep
  (lambda (n)
    (cond
     ((zero? n)(quote pizza))
     (else (cons (deep (sub1 n))
                 (quote ()))))))

(deep 3)
; -> (((pizza)))


;; Ns, Rs

(define Ns (quote ()))

(define deepR
  (lambda (n)
    (set! Ns (cons n Ns))
    (deep n)))

(deepR 3)
; -> (((pizza)))

Ns
; -> (3)


(define Rs (quote ()))
(define Ns (quote ()))

(define deepR
  (lambda (n)
    (set! Rs (cons (deep n) Rs))
    (set! Ns (cons n Ns))
    (deep n)))

(deepR 3)
; -> (((pizza)))

Ns
; -> (3)

Rs
; -> ((((pizza))))

(deepR 5)
; -> (((((pizza)))))

Ns
; -> (5 3)

Rs
; -> ((((((pizza))))) (((pizza))))

(deepR 3)
; -> (((pizza)))

Ns
; -> (3 5 3)

Rs
; -> ((((pizza))) (((((pizza))))) (((pizza))))

;; ((((pizza)))
;;  (((((pizza)))))
;;  (((pizza))))


;; find

;; (find 3 Ns Rs)
;; -> (((pizza)))
;; (find 5 Ns Rs)
;; -> (((((pizza)))))

(define (find n ns rs)
  (cond
   ((or (null? ns)
        (null? rs)) #f)
   ((= n (car ns))(car rs))
   (else (find (- n 1)(cdr ns)(cdr rs)))))

(find 3 Ns Rs)
; -> (((pizza)))


;; The Seasoned Schemer
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
               ((= (car ns) n)(car rs))
               (else (A (cdr ns)(cdr rs)))))))
      (A Ns Rs))))

(find 5 Ns Rs)
; -> (((((pizza)))))


;; deepM

(define (member? a lat)
  (let/cc skip
    (fold (lambda (e acc)
            (if (eq? e a)
                (skip #t)
                acc))
          #f lat)))

(member? 1 '(a b c))
; -> #f
(member? 'c '(a b c d e))
; -> #t


(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n))))

Ns
; -> (3 5 3)

Rs
; -> ((((pizza))) (((((pizza))))) (((pizza))))

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

Ns
; -> (5 3)

Rs
; -> ((((((pizza))))) (((pizza))))

(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))


; (deep 6) -> (cons (deep 5)(quote ()))

(define deep
  (lambda (m)
    (cond
     ((zero? m)(quote pizza))
     (else (cons (deepM (sub1 m))
                 (quote ()))))))

(deep 6)
; -> ((((((pizza))))))

Ns
; -> (5 3)

(deep 9)
; -> (((((((((pizza)))))))))

Ns
; -> (8 7 6 5 3)


(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(deepM 16)
; -> ((((((((((((((((pizza))))))))))))))))


(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
               ((null? ns) #f)
               ((= (car ns) n)(car rs))
               (else (A (cdr ns)(cdr rs)))))))
      (A Ns Rs))))

(define (atom? a)
  (and (not (pair? a))
       (not (null? a))))

(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (if (atom? (find n Ns Rs))
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          (find n Ns Rs)))))

(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))


;; length

(define add1
  (lambda (n)
    (+ n 1)))

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))


(define length
  (lambda (l)
    0))

(set! length
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))


(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
             ((null? l) 0)
             (else (add1 (h (cdr l)))))))
    h))

(length '(1 2 3))
; -> 3


; ?
(define length
  (let ((h #f))
    (set! h (lambda (l)
              (if (null? l)
                  0
                  (add1 (h (cdr l))))))))

(length '(1 2 3 4 5))
; -> 5


(define L
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))

(define length
  (let ((h (lambda (l)
             0)))
    (set! h
          (L (lambda (arg)
               (h arg))))))


;; Y!

(define Y!
  (lambda (L)
    (let ((h (lambda (l)
               (quote ()))))
      (set! h
            (L (lambda (arg)
                 (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (arg)
                 (h arg)))))
      h)))

((Y! L) '(1 2 3))
; -> 3



;; D

(define D
  (lambda (depth*)
    (lambda (s)
      (cond
       ((null? s) 1)
       ((atom? (car s))
        (depth* (cdr s)))
       (else (max
              (add1 (depth* (car s)))
              (depth* (cdr s))))))))

(define depth* (Y! D))

(depth* '(1 (2 (3 (4 (5 (6 (7 (8)))))))))
; -> 8


;; biz

(define biz
  (lambda (f)
    (let ((x 0))
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

