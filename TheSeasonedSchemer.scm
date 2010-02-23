;;; the seasoned schemer

;; capter.11

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


;; scramble


; (1 1 1 3 4 2 1 1 9 2) -> (1 1 1 1 1 4 1 1 1 9)
; (1 2 3 4 5 6 7 8 9) -> (1 1 1 1 1 1 1 1 1)
; (1 2 3 1 2 3 4 1 8 2 10) -> (1 1 1 1 1 1 1 1 2 8 2)

(define one?
  (lambda (n)
    (= n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define pick
  (lambda (n lat)
    (if (one? n)
        (car lat)
        (pick (sub1 n)(cdr lat)))))

(define pick
  (lambda (n lat)
    (list-ref lat (sub1 n))))

(pick 3 '(a b c d e f g))
; -> c

(define scramble-b
  (lambda (tup rev-pre)
    (if (null? tup)
        '()
        (cons (pick (car tup)
                    (cons (car tup) rev-pre))
              (scramble-b (cdr tup)
                          (cons (car tup) rev-pre))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(define scramble-b
  (lambda (tup rev-pre)
    (if (null? tup)
        '()
        (let* ((n (car tup))
               (rev (cons n rev-pre)))
          (display (format "n = ~a, ret = ~a, tup = ~a, rev-pre = ~a\n"
                           n (pick n rev) tup rev-pre))
          (cons (pick n rev)
                (scramble-b (cdr tup) rev))))))

; (1 1 1 3 4 2 1 1 9 2) -> (1 1 1 1 1 4 1 1 1 9)
(scramble '(1 1 1 3 4 2 1 1 9 2))
;; n = 1, ret = 1, tup = (1 1 1 3 4 2 1 1 9 2), rev-pre = ()
;; n = 1, ret = 1, tup = (1 1 3 4 2 1 1 9 2), rev-pre = (1)
;; n = 1, ret = 1, tup = (1 3 4 2 1 1 9 2), rev-pre = (1 1)
;; n = 3, ret = 1, tup = (3 4 2 1 1 9 2), rev-pre = (1 1 1)
;; n = 4, ret = 1, tup = (4 2 1 1 9 2), rev-pre = (3 1 1 1)
;; n = 2, ret = 4, tup = (2 1 1 9 2), rev-pre = (4 3 1 1 1)
;; n = 1, ret = 1, tup = (1 1 9 2), rev-pre = (2 4 3 1 1 1)
;; n = 1, ret = 1, tup = (1 9 2), rev-pre = (1 2 4 3 1 1 1)
;; n = 9, ret = 1, tup = (9 2), rev-pre = (1 1 2 4 3 1 1 1)
;; n = 2, ret = 9, tup = (2), rev-pre = (9 1 1 2 4 3 1 1 1)
;; (1 1 1 1 1 4 1 1 1 9)

; (1 2 3 4 5 6 7 8 9) -> (1 1 1 1 1 1 1 1 1)
(scramble '(1 2 3 4 5 6 7 8 9))
;; n = 1, ret = 1, tup = (1 2 3 4 5 6 7 8 9), rev-pre = ()
;; n = 2, ret = 1, tup = (2 3 4 5 6 7 8 9), rev-pre = (1)
;; n = 3, ret = 1, tup = (3 4 5 6 7 8 9), rev-pre = (2 1)
;; n = 4, ret = 1, tup = (4 5 6 7 8 9), rev-pre = (3 2 1)
;; n = 5, ret = 1, tup = (5 6 7 8 9), rev-pre = (4 3 2 1)
;; n = 6, ret = 1, tup = (6 7 8 9), rev-pre = (5 4 3 2 1)
;; n = 7, ret = 1, tup = (7 8 9), rev-pre = (6 5 4 3 2 1)
;; n = 8, ret = 1, tup = (8 9), rev-pre = (7 6 5 4 3 2 1)
;; n = 9, ret = 1, tup = (9), rev-pre = (8 7 6 5 4 3 2 1)
;; (1 1 1 1 1 1 1 1 1)

; (1 2 3 1 2 3 4 1 8 2 10) -> (1 1 1 1 1 1 1 1 2 8 2)
(scramble '(1 2 3 1 2 3 4 1 8 2 10))
;; n = 1, ret = 1, tup = (1 2 3 1 2 3 4 1 8 2 10), rev-pre = ()
;; n = 2, ret = 1, tup = (2 3 1 2 3 4 1 8 2 10), rev-pre = (1)
;; n = 3, ret = 1, tup = (3 1 2 3 4 1 8 2 10), rev-pre = (2 1)
;; n = 1, ret = 1, tup = (1 2 3 4 1 8 2 10), rev-pre = (3 2 1)
;; n = 2, ret = 1, tup = (2 3 4 1 8 2 10), rev-pre = (1 3 2 1)
;; n = 3, ret = 1, tup = (3 4 1 8 2 10), rev-pre = (2 1 3 2 1)
;; n = 4, ret = 1, tup = (4 1 8 2 10), rev-pre = (3 2 1 3 2 1)
;; n = 1, ret = 1, tup = (1 8 2 10), rev-pre = (4 3 2 1 3 2 1)
;; n = 8, ret = 2, tup = (8 2 10), rev-pre = (1 4 3 2 1 3 2 1)
;; n = 2, ret = 8, tup = (2 10), rev-pre = (8 1 4 3 2 1 3 2 1)
;; n = 10, ret = 2, tup = (10), rev-pre = (2 8 1 4 3 2 1 3 2 1)
;; (1 1 1 1 1 1 1 1 2 8 2)


(define scramble
  (lambda (tup)
    (letrec ((iter (lambda (t rev-pre)
                     (if (null? t)
                         '()
                         (let* ((n (car t))
                                (rev (cons n rev-pre)))
                           (cons (pick n rev)
                                 (iter (cdr t) rev)))))))
      (iter tup '()))))


;; chapter.12

; multirember

; standard
(define (multirember a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat))(multirember a (cdr lat)))
        (else (cons (car lat)
                    (multirember a (cdr lat))))))

(multirember 'a '(a b c d a b c d))
      

; fold
(define (multirember a lat)
  (fold (lambda (e l)
          (if (eq? a e)
              l
              (cons e l)))
        '()
        lat))

(multirember 1 '(1 2 3 1 2 3 1 2 3))


; the seasoned schemer
; y combinator
(define Y
  (lambda (f)
    ((lambda (g)
       (g g))
     (lambda (h)
       (f (lambda (x)
            ((h h) x)))))))

((Y (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n (f (- n 1))))))) 5)

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond ((null? lat) '())
                  ((eq? a (car lat))(mr (cdr lat)))
                  (else (cons (car lat)
                              (mr (cdr lat))))))))
     lat)))

(multirember 'a '(a b c a b c a b c))

(define multirember
  (lambda (a lat)
    (((lambda (f)
        ((lambda (g)
           (g g))
         (lambda (h)
           (f (lambda (x)
                ((h h) x))))))
      (lambda (mr)
        (lambda (lat)
          (cond ((null? lat) '())
                ((eq? a (car lat))(mr (cdr lat)))
                (else (cons (car lat)
                            (mr (cdr lat))))))))
     lat)))

(multirember 1 '(1 2 1 2 1 2 3))


;; length

(define add1
  (lambda (n)
    (+ n 1)))

;; (define length 'hoge)
;; (display length)

;; (define length
;;   (with-module gauche length))

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))))

(length '(1 2 3))


; letrec

(define multirember
  (lambda (a lat)
    (letrec ((mr (lambda (l)
                   (cond ((null? l) '())
                         ((eq? a (car l))(mr (cdr l)))
                         (else (cons (car l)
                                     (mr (cdr l))))))))
      (mr lat))))

(multirember 'a '(a b a b a b c a))
