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
