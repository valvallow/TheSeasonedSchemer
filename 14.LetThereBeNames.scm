;; capter.14


; leftmost

;; (leftmost '(((a) b)(c d)))
;;  -> a

; The Seasoned Schemer
(define atom?
  (lambda (a)
    (and (not (pair? a))
         (not (null? a)))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l))(car l))
     (else (leftmost (car l))))))

(leftmost '(((a) b)(c d)))
; -> a
(leftmost '(((a)())()(e)))
; -> a

(leftmost '(((() a)())))
; -> *** ERROR: pair required, but got ()


; leftmost * The Seasoned Schemer
(define leftmost
  (lambda (l)
    (cond
     ((null? l)(quote ()))
     ((atom? (car l))(car l))
     (else (cond
            ((atom? (leftmost (car l)))
             (leftmost (car l)))
            (else (leftmost (cdr l))))))))

(leftmost '(((a) b)(c d)))
; -> a
(leftmost '(((a)())()(e)))
; -> a
(leftmost '(((() a)())))
; -> a


(define leftmost
  (lambda (l)
    (cond
     ((null? l)(quote ()))
     ((atom? (car l))(car l))
     (else (let ((a (leftmost (car l))))
             (cond
              ((atom? a) a)
              (else (leftmost (cdr l)))))))))

(leftmost '(((a) b)(c d)))
; -> a
(leftmost '(((a)())()(e)))
; -> a
(leftmost '(((() a)())))
; -> a


(use srfi-1)
(define (eqlist? l1 l2)
  (list= eq? l1 l2))


; rember1*

;; (rember1* 'salad '((Swedish rye)
;;                    (French (mustard salad turkey))
;;                    salad))
;;  -> ((Swedish rye)
;;      (French (mustard turkey))
;;      salad)


; The Seasoned Schemer
(define rember1*
  (lambda (a lat)
    (cond
     ((null? lat)(quote ()))
     ((atom? (car lat))
      (cond
       ((eq? (car lat) a)(cdr lat))
       (else (cons (car lat)
                   (rember1* a (cdr lat))))))
     (else
      (cond
       ((eqlist?
         (rember1* a (car lat)) ; *
         (car lat))
        (cons (car lat)
              (rember1* a (cdr lat))))
       (else (cons (rember1* a (car lat)) ; *
                   (cdr lat))))))))

(rember1* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   salad))
; -> ((Swedish rye) (French (mustard turkey)) salad)
                      
; The Seasoned Schemer letrec
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l)(quote ()))
               ((atom? (car l))
                (cond
                 ((eq? (car l) a)(cdr l))
                 (else (cons (car l)
                             (R (cdr l))))))
               (else
                (cond
                 ((eqlist?
                   (R (car l)) ; *
                   (car l))
                  (cons (car l)
                        (R (cdr l))))
                 (else (cons (R (car l)) ; *
                             (cdr l)))))))))
      (R l))))
                 
(rember1* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   salad))
; -> ((Swedish rye) (French (mustard turkey)) salad)


; The Seasoned Schemer letrec, let
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l)(quote ()))
               ((atom? (car l))
                (cond
                 ((eq? (car l) a)(cdr l))
                 (else (cons (car l)
                             (R (cdr l))))))
               (else (let ((av (R (car l))))
                       (cond
                        ((eqlist? (car l) av)
                         (cons (car l)(R (cdr l))))
                        (else (cons av (cdr l))))))))))
      (R l))))

(rember1* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   salad))
; -> ((Swedish rye) (French (mustard turkey)) salad)


; rember*

; fold-right
(use srfi-1)
(define (rember* a l)
  (fold-right (lambda (e acc)
                (if (list? e)
                    (cons (rember* a e)
                          acc)
                    (if (eq? a e)
                        acc
                        (cons e
                              acc))))
              '() l))

(rember* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   salad))

(define (rember* a l)
  (fold-right (lambda (e acc)
                (let ((kons (lambda (kar)
                              (cons kar acc))))
                  (if (list? e)
                      (kons (rember* a e))
                      (if (eq? a e)
                          acc
                          (kons e)))))
              '() l))

(rember* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   salad))

(define (rember* a l)
  (fold-right (lambda (e acc)
                (if (eq? a e)
                    acc
                    (cons (if (list? e)
                              (rember* a e)
                              e)
                          acc)))
              '() l))


(rember* 'salad '((Swedish rye)
                   (French (mustard salad turkey))
                   salad))


; depth*

; (define max (with-module gauche max))

(define (depth* l)
  (if (null? l)
      1
      (let ((kar (car l))
            (d (depth* (cdr l))))
        (if (list? kar)
            (max (+ 1 (depth* kar))
                 d)
            d))))

; fold
(define (depth* l)
  (fold (lambda (e acc)
          (max (if (list? e)
                   (+ 1 (depth* e))
                   acc)
               acc))
        1 l))



(depth* '((pickled) peppers (peppers pickled)))
; -> 2
(depth* '())
; -> 1
(depth* '(()
          ((bitter butter)
           (makes)
           (batter
            (bitter)))
          butter))
; -> 4


; The Seasoned Schemer
(define add1
  (lambda (n)
    (+ n 1)))

(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth* (cdr l)))
     (else
      (cond
       ((> (depth* (cdr l))
           (add1 (depth* (car l))))
        (depth* (cdr l)))
       (else
        (add1 (depth* (car l)))))))))

(depth* '((pickled) peppers (peppers pickled)))
; -> 2
(depth* '())
; -> 1
(depth* '(()
          ((bitter butter)
           (makes)
           (batter
            (bitter)))
          butter))
; -> 4

; The Seasoned Schemer letting
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth* (cdr l)))
     (else
      (let ((a (add1 (depth* (car l))))
            (d (depth* (cdr l))))
        (cond
         ((> d a) d)
         (else a)))))))

(depth* '((pickled) peppers (peppers pickled)))
; -> 2
(depth* '())
; -> 1
(depth* '(()
          ((bitter butter)
           (makes)
           (batter
            (bitter)))
          butter))
; -> 4


; The Seasoned Schemer, if
(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth* (cdr l)))
     (else (max
            (add1 (depth* (car l)))
            (depth* (cdr l)))))))
      
(depth* '((pickled) peppers (peppers pickled)))
; -> 2
(depth* '())
; -> 1
(depth* '(()
          ((bitter butter)
           (makes)
           (batter
            (bitter)))
          butter))
; -> 4


; letting excersise scramble

(define (pick n lat)
  (list-ref lat (- n 1)))

; The Seasoned Schemer
(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
               ((null? tup)(quote ()))
               (else
                (let ((rp (cons (car tup) rp)))
                  (cons (pick (car tup) rp)
                        (P (cdr tup) rp))))))))
      (P tup  (quote ())))))

(define (scramble tup)
  (letrec
      ((P (lambda (tup rp)
            (if (null? tup)
                '()
                (let ((rp (cons (car tup) rp)))
                  (cons (pick (car tup) rp)
                        (P (cdr tup) rp)))))))
    (P tup '())))

; again
(define (scramble tup)
  (letrec
      ((P (lambda (tup rp)
            (if (null? tup)
                '()
                (let ((kar (car tup)))
                  (let ((rp (cons kar rp)))
                    (cons (pick kar rp)
                          (P (cdr tup) rp))))))))
    (P tup '())))

; named let
(define (scramble tup)
  (let loop ((tup tup)
             (rp '()))
    (if (null? tup)
        '()
        (let ((kar (car tup)))
          (let ((rp (cons kar rp)))
            (cons (pick kar rp)
                  (loop (cdr tup) rp)))))))

; fold2
(use gauche.collection)
(define (scramble tup)
  (reverse
   (fold2 (lambda (e acc rp)
            (let ((rp (cons e rp)))
              (values (cons (list-ref rp (- e 1))
                            acc)
                      rp)))
          '() '() tup))
         
(scramble '(1 2 3 4 5 6 7 8 9))
;; (1 1 1 1 1 1 1 1 1)
;; (9 8 7 6 5 4 3 2 1)
(scramble '(1 1 1 3 4 2 1 1 9 2))
;; (9 1 1 1 4 1 1 1 1 1)
;; (2 9 1 1 2 4 3 1 1 1)
(scramble '(1 2 3 1 2 3 4 1 8 2 10))
;; (2 8 2 1 1 1 1 1 1 1 1)
;; (10 2 8 1 4 3 2 1 3 2 1)


; letcc leftmost

; The Seasoned Schemer
(define leftmost
  (lambda (l)
    (let/cc skip
      (lm l skip))))

(define lm
  (lambda (l out)
    (cond
     ((null? l)(quote ()))
     ((atom? (car l))(out (car l)))
     (else (let ()
             (lm (car l) out)
             (lm (cdr l) out))))))

(leftmost '(((a) b (c))))
; -> a


; again
(define leftmost
  (letrec
      ((lm (lambda (l out)
             (cond
              ((null? l)(quote ()))
              ((atom? (car l))
               (out (car l)))
              (else
               (let ()
                 (lm (car l) out)
                 (lm (cdr l) out)))))))
    (lambda (l)
      (let/cc skip
        (lm l skip)))))

(leftmost '(((a) b (c))))

; again
(define leftmost
  (lambda (l)
    (letrec
        ((lm (lambda (l out)
               (cond
                ((null? l)(quote ()))
                ((atom? (car l))
                 (out (car l)))
                (else
                 (let ()
                   (lm (car l) out)
                   (lm (cdr l) out)))))))
      (let/cc skip
        (lm l skip)))))

(leftmost '(((a) b (c))))

; again
(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l out)
                 (cond
                  ((null? l)(quote ()))
                  ((atom? (car l))
                   (out (car l)))
                  (else (let ()
                          (lm (car l) out)
                          (lm (cdr l) out)))))))
        (lm l skip)))))

(leftmost '(((a) b (c))))

; again
(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                  ((null? l)(quote ()))
                  ((atom? (car l))
                   (skip (car l)))
                  (else (let ()
                          (lm (car l))
                          (lm (cdr l))))))))
        (lm l)))))

(leftmost '(((a) b (c))))


(define (leftmost l)
  (let/cc skip
    (letrec
        ((lm (lambda (l)
               (if (null? l)
                   '()
                   (let ((kar (car l)))
                     (if (list? kar)
                         (let ()
                           (lm kar)
                           (lm (cdr l)))
                         (skip kar)))))))
      (lm l))))

(leftmost '(((a) b (c))))

; named let
(define (leftmost l)
  (let/cc skip
    (let loop ((l l))
      (if (null? l)
          '()
          (let ((kar (car l)))
            (if (list? kar)
                (let () ; begin
                  (loop kar)
                  (loop (cdr l)))
                (skip kar)))))))


(leftmost '(((a) b (c))))
(leftmost '((() (a) b (c)))
