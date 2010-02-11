;;; the seasoned schemer

;; member?

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          ((eq? a (car lat)) #t)
          (else (member? a (cdr lat))))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat))
                    (member? a (cdr lat)))))))

(define member?
  (lambda (a lat)
    (eq? a (find (lambda (e)
                   (eq? a e)) lat))))

(define member?
  (lambda (a lat)
    (any (lambda (e)
           (eq? a e)) lat)))
  
(member? 'sardines '(Italian sardines spaghetti parsley))


;; two-in-a-row?

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          ((member? (car lat)(cdr lat)) #t)
          (else (two-in-a-row? (cdr lat))))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (or (member? (car lat)(cdr lat))
                    (two-in-a-row? (cdr lat)))))))

(two-in-a-row? '(a b c d e d f g))

(cond (#f => (lambda (b)
               (not b)))
      ((list 'a 'b) => (lambda (a)
                         (display (cadr a))))
      (else 0))


;; sum-of-prefixes


; (2 1 9 17 0) -> (2 3 12 29 29)
; (1 1 1 1 1) -> (1 2 3 4 5)


; letrec
(define sum-of-prefixes
  (lambda (tup)
    (letrec ((f (lambda  (t p)
                  (if (null? t)
                      '()
                      (let ((v (+ (car t) p)))
                        (cons v
                              (f (cdr t) v)))))))
      (f tup 0))))

; fold
(define sum-of-prefixes
  (lambda (tup)
    (reverse (fold (lambda (e r)
                     (cons (+ e (if (null? r)
                                    0
                                    (car r)))
                           r))
                   '()
                   tup))))

; pair-fold
(use srfi-1)
(define sum-of-prefixes
  (lambda (tup)
    (reverse (pair-fold (lambda (rest ret)
                          (cons (+ (car rest)
                                   (if (null? ret)
                                       0
                                       (car ret)))
                                ret))
                        '()
                        tup))))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (if (null? tup)
        '()
        (let ((a (+ sonssf (car tup))))
          (cons a (sum-of-prefixes-b a (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(sum-of-prefixes '(2 1 9 17 0))
; -> (2 3 12 29 29)
(sum-of-prefixes '(1 1 1 1 1))
; -> (1 2 3 4 5)
