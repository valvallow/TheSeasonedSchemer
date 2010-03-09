;;; the Seasoned schemer

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


; filter
(use srfi-1)
(define (multirember a lat)
  (filter (lambda (e)
            (not (eq? a e)))
          lat))

(multirember 'a '(a b c a b c))



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


; multirember

; How To Become A Hacker:http://beta-reduction.blogspot.com/2010/02/define-multirember-lat-cond-null-lat-eq.html
(define (multirember a lat)          
  (if (null? lat)           
      '()
      (let ((it (car lat))
            (recur (multirember a (cdr lat))))
        (if (eq? a it)
            recur
            (cons it recur)))))

(multirember 'a '(a a a b b b c c  c a a a))

; call/cc - http://beta-reduction.blogspot.com/2010/02/define-multirember-lat-cond-null-lat-eq.html
(define (multirember a lat)
  (call/cc
   (lambda (k)
     (let ((it (car (if (null? lat)
                        (k '())
                        lat)))
           (recur (multirember a (cdr lat))))
       (if (eq? a it)
           recur
           (cons it recur))))))

(multirember 1 '(1 2 1 2 3 4  5 5 61 1 1 ))


; rember-f - The Little Schemer
(define rember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? a (car lat))(cdr lat))
            (else (cons (car lat)
                        ((rember-f test?) a (cdr lat))))))))

(define rem-eq?
  (rember-f eq?))

(rem-eq? 'a '(b a b a b))


; multirember -> multirember-f

; The Seasoned Schemer
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? a (car lat))((multirember-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'a '(a b c a b c))

; fold
(define (multirember-f test?)
  (lambda (a lat)
    (fold (lambda (e l)
            (if (test? a e)
                l
                (cons e l)))
          '()
          lat)))

((multirember-f eq?) 'a '(a b c a b c))

; http://beta-reduction.blogspot.com/2010/02/define-multirember-lat-cond-null-lat-eq.html
(define (multirember-f test?)
  (lambda (a lat)
    (if (null? lat)
        '()
        (let ((b (car lat))
              (mr ((multirember-f test?) a (cdr lat))))
          (if (test? a b)
              mr
              (cons b mr))))))

((multirember-f eq?) 'a '(a b c d a b c d))


; letrec multirember-f - The Seasoned Schemer
(define multirember-f
  (lambda (test?)
    (letrec ((m-f (lambda (a lat)
                    (cond ((null? lat) '())
                          ((test? a (car lat))(m-f a (cdr lat)))
                          (else (cons (car lat)
                                      (m-f a (cdr lat))))))))
      m-f)))

((multirember-f eq?) 1 '(1 2 3 4 5 1 2 3 1 2 3 1 2))


; letrec multirember - The Seasoned Schemer
(define multirember
  (letrec ((multirember
            (lambda (a lat)
              (cond ((null? lat) '())
                    ((eq? a (car lat))(multirember a (cdr lat)))
                    (else (cons (car lat)
                                (multirember a (cdr lat))))))))
    multirember))

(multirember 'a '(a b a b a b c a))



; member?

; The Little Schemer
(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          ((eq? a (car lat)) #t)
          (else (member? a (cdr lat))))))

(member? 'b '(a b c))
(member? 'd '(a b c))

(define member?
  (lambda (a lat)
    (cond ((or (null? lat)
               (not (eq? a (car lat)))) #f)
          (else (member? a (cdr lat))))))

(member? 3 '(1 2 3 4 5))
(member? 6 '(1 2 3 4 5))

; and or
(define (member? a lat)
  (and (not (null? lat))
       (or (eq? a (car lat))
           (member? a (cdr lat)))))

(member? 'c '(a b c d e f))
(member? 1 '(a b c d e f))

; any
(use srfi-1)
(define (member? a lat)
  (any (lambda (e)
         (eq? e a))
       lat))

(member? 'a '(a b c))
(member? 'd '(a b c))


; The Seasoned Schemer

(define member?
  (lambda (a lat)
    ((letrec ((yes? (lambda (l)
                      (cond ((null? l) #f)
                            ((eq? a (car l)) #t)
                            (else (yes? (cdr l)))))))
       yes?)
     lat)))

(member? 'a '(a b c))
(member? 'd '(a b c))


(define member?
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
                     (cond ((null? l) #f)
                           ((eq? a (car l)) #t)
                           (else (member? a (cdr l)))))))
      (yes? lat))))

(member? 'a '(a b c))
(member? 'd '(a b c))


; union

; The Little Schemer
(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))

(union '(1 2 3 4 5 6) '(3 4 5 6 7 8 9 10))
(union '(tomatos and macaroni casserole) '(macaroni and cheese))

; fold-right
(define (union set1 set2)
  (fold-right (lambda (e l)
                (if (member? e set2)
                    l
                    (cons e l)))
              set2
              set1))

(union '(1 2 3 4 5 6) '(3 4 5 6 7 8 9 10))
(union '(tomatos and macaroni casserole) '(macaroni and cheese))


; The Seasoned Schemer
(define union
  (lambda (set1 set2)
    (letrec ((U (lambda (set)
                  (cond ((null? set) set2)
                        ((member? (car set) set2)
                         (U (cdr set)))
                        (else (cons (car set)
                                    (U (cdr set))))))))
      (U set1))))

(union '(1 2 3 4 5 6) '(3 4 5 6 7 8 9 10))
(union '(tomatos and macaroni casserole) '(macaroni and cheese))


(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((M? (car set) set2)
                     (U (cdr set)))
                    (else (cons (car set)
                                (U (cdr set)))))))
         (M? (lambda (a lat)
               (cond ((null? lat) #f)
                     ((eq? a (car lat)) #t)
                     (else (M? a (cdr lat)))))))
      (U set1))))

(union '(1 2 3 4 5 6) '(3 4 5 6 7 8 9 10))
(union '(tomatos and macaroni casserole) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond ((null? set) set2)
                    ((M? (car set) set2)
                     (U (cdr set)))
                    (else (cons (car set)
                                (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (l)
                          (cond ((null? l) #f)
                                ((eq? a (car l)) #t)
                                (else (N? (cdr l)))))))
                 (N? lat)))))
      (U set1))))

(union '(1 2 3 4 5 6) '(3 4 5 6 7 8 9 10))
(union '(tomatos and macaroni casserole) '(macaroni and cheese))


; two-in-a-row

(define two-in-a-row?
  (lambda (lat)
    (letrec
        ((W (lambda (a lat)
              (cond ((null? lat) #f)
                    (else (or (eq? a (car lat))
                              (W (car lat)(cdr lat))))))))
      (cond ((null? lat) #f)
            (else (W (car lat)(cdr lat)))))))

(two-in-a-row? '(a b c c d e))
(two-in-a-row? '(a b c  d e f))

(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond ((null? lat) #f)
                  (else (or (eq? a (car lat))
                            (W (car lat)(cdr lat))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else (W (car lat)(cdr lat)))))))

(two-in-a-row? '(a b c c d e))
(two-in-a-row? '(a b c  d e f))

(define sum-of-prefixes
  (lambda (tup)
    (letrec ((S (lambda (sss tup)
                  (cond ((null? tup) '())
                        (else (cons (+ sss (car tup))
                                    (S (+ sss (car tup))
                                       (cdr tup))))))))
      (S 0 tup))))

(sum-of-prefixes '(1 2 3 4 5))

(define sum-of-prefixes
  (lambda (tup)
    (reverse
     (fold (lambda (e l)
             (cons (if (null? l)
                       e
                       (+ e (car l)))
                   l))
           '()
           tup))))

(sum-of-prefixes '(1 2 3 4 5))


; scramble

(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (t rp)
              (cond ((null? t) '())
                    (else (cons (pick (car t)
                                      (cons (car t) rp))
                                (P (cdr t)
                                   (cons (car t) rp))))))))
      (P tup '()))))

(scramble '(1 2 3 4 5 6 7 8 9))


(define scramble
  (letrec
      ((P (lambda (t rp)
            (cond ((null? t) '())
                  (else (cons (pick (car t)
                                    (cons (car t) rp))
                              (P (cdr t)
                                 (cons (car t) rp))))))))
    (lambda (tup)
      (P tup '()))))

(scramble '(1 2 3 4 5 6 7 8 9))


; intersect

; (intersect '(tomatoes and macaroni) '(macaroni and cheese))
; -> (and macaroni)

; normal recur
(define (intersect set1 set2)
  (cond ((or (null? set1)
             (null? set2)) '())
        ((member? (car set1) set2) (cons (car set1)
                                         (intersect (cdr set1) set2)))
        (else (intersect (cdr set1) set2))))

(intersect '(tomatoes and macaroni) '(macaroni and cheese))
;  -> (and macaroni)


; normal recur
(define (intersect set1 set2)
  (if (or (null? set1)
          (null? set2))
      '()
      (let ((a (car set1))
            (n (intersect (cdr set1) set2)))
        (if (member? a set2)
            (cons a n)
            n))))

(intersect '(tomatoes and macaroni) '(macaroni and cheese))
; -> (and macaroni)


; fold-right
(use srfi-1)
(define (intersect set1 set2)
  (fold-right (lambda (e acc)
                (if (member? e set2)
                    (cons e acc)
                    acc))
              '()
              set1))

(intersect '(tomatoes and macaroni) '(macaroni and cheese))
; -> (and macaroni)


; letrec
(define (intersect set1 set2)
  (letrec
      ((int (lambda (s acc)
              (cond ((null? s) '())
                    ((member? (car s) set2)
                     (cons (car s)(int (cdr s) acc)))
                    (else (int (cdr s) acc))))))
    (if (or (null? set1)
            (null? set2))
        '()
        (int set1 '()))))

(intersect '(tomatoes and macaroni) '(macaroni and cheese))
; -> (and macaroni)

; letrec
(define (intersect set1 set2)
  (letrec
      ((int (lambda (s acc)
              (if (null? s)
                  acc
                  (let ((a (car s))
                        (n (int (cdr s) acc)))
                    (if (member? a set2)
                        (cons a n)
                        n))))))
    (if (or (null? set1)
            (null? set2))
        '()
        (int set1 '()))))
                   
(intersect '(tomatoes and macaroni) '(macaroni and cheese))
; -> (and macaroni)


; The Seasoned Schemer
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1)(quote ()))
     ((member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(intersect '(tomatoes and macaroni) '(macaroni and cheese))
; -> (and macaroni)

; The Seasoned Schemer letrec
(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
               ((null? set)(quote ()))
               ((member? (car set) set2)
                (cons (car set)
                      (I (cdr set))))
               (else (I (cdr set)))))))
      (I set1))))
(intersect '(tomatoes and macaroni) '(macaroni and cheese))
; -> (and macaroni)
