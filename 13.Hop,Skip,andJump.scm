;; chapter.13

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



; intersectall

; The Seasoned Schemer

(define intersectall
  (lambda (lset)
    (cond
     ((null? lset) '())
     ((null? (cdr lset))(car lset))
     (else (intersect (car lset)
                      (intersectall (cdr lset)))))))

(intersectall '((tomatoes and macaroni)
                (macaroni and cheese)))
; -> (and macaroni)

; The Seasoned Schemer letrec
(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
               ((null? (cdr lset))
                (car lset))
               (else (intersect (car lset)
                                (A (cdr lset))))))))
      (cond
       ((null? lset)(quote ()))
       (else (A lset))))))

(intersectall '((tomatoes and macaroni)
                (macaroni and cheese)))
; -> (and macaroni)


; The Seasoned Schemer letcc

; letcc -> gauche let/cc
(define letcc let/cc)

(define intersectall
  (lambda (lset)
    (let/cc hop ; L:(catch 'hop)
      (letrec
          ((A (lambda (lset)
                (cond
                 ((null? #?=(car lset))
                  (hop (quote ()))) ; L:(throw 'hop (quote ()))
                 ((null? (cdr lset))
                  (car lset))
                 (else
                  (intersect (car lset)
                             (A (cdr lset))))))))
        (cond
         ((null? lset)(quote ()))
         (else (A lset)))))))

(intersectall '((3 mangos and)
                (3 kiwis and)
                (3 hamburgers)))
;; #?="(stdin)":252:(car lset)
;; #?-    (3 mangos and)
;; #?="(stdin)":252:(car lset)
;; #?-    (3 kiwis and)
;; #?="(stdin)":252:(car lset)
;; #?-    (3 hamburgers)
;; (3)
                
(intersectall '((3 steaks and)
                (no food and)
                (three baked potatoes)
                (3 diet hamburgers)))
;; #?="(stdin)":252:(car lset)
;; #?-    (3 steaks and)
;; #?="(stdin)":252:(car lset)
;; #?-    (no food and)
;; #?="(stdin)":252:(car lset)
;; #?-    (three baked potatoes)
;; #?="(stdin)":252:(car lset)
;; #?-    (3 diet hamburgers)
;; ()

(intersectall '((3 mangoes and)
                ()
                (3 diet hamburgers)))
;; #?="(stdin)":252:(car lset)
;; #?-    (3 mangoes and)
;; #?="(stdin)":252:(car lset)
;; #?-    ()
;; ()


; letcc -> call-with-current-continuation
(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                  ((null? #?=(car lset))
                   (hop (quote ())))
                  ((null? (cdr lset))
                   (car lset))
                  (else
                   (intersect (car lset)
                              (A (cdr lset))))))))
         (cond
          ((null? lset)(quote ()))
          (else (A lset))))))))

(intersectall '((3 mangos and)
                (3 kiwis and)
                (3 hamburgers)))
;; #?="(stdin)":279:(car lset)
;; #?-    (3 mangos and)
;; #?="(stdin)":279:(car lset)
;; #?-    (3 kiwis and)
;; #?="(stdin)":279:(car lset)
;; #?-    (3 hamburgers)
;; (3)
                
(intersectall '((3 steaks and)
                (no food and)
                (three baked potatoes)
                (3 diet hamburgers)))
;; #?="(stdin)":279:(car lset)
;; #?-    (3 steaks and)
;; #?="(stdin)":279:(car lset)
;; #?-    (no food and)
;; #?="(stdin)":279:(car lset)
;; #?-    (three baked potatoes)
;; #?="(stdin)":279:(car lset)
;; #?-    (3 diet hamburgers)
;; ()

(intersectall '((3 mangoes and)
                ()
                (3 diet hamburgers)))
;; #?="(stdin)":279:(car lset)
;; #?-    (3 mangoes and)
;; #?="(stdin)":279:(car lset)
;; #?-    ()
;; ()

(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
               ((null? #?=(car lset))
                '())
               ((null? (cdr lset))
                (car lset))
               (else
                (intersect (car lset)
                           (A (cdr lset))))))))
      (cond
       ((null? lset)(quote ()))
       (else (A lset))))))

(intersectall '((3 mangos and)
                (3 kiwis and)
                (3 hamburgers)))
;; #?="(stdin)":323:(car lset)
;; #?-    (3 mangos and)
;; #?="(stdin)":323:(car lset)
;; #?-    (3 kiwis and)
;; #?="(stdin)":323:(car lset)
;; #?-    (3 hamburgers)
;; (3)
                
(intersectall '((3 steaks and)
                (no food and)
                (three baked potatoes)
                (3 diet hamburgers)))
;; #?="(stdin)":323:(car lset)
;; #?-    (3 steaks and)
;; #?="(stdin)":323:(car lset)
;; #?-    (no food and)
;; #?="(stdin)":323:(car lset)
;; #?-    (three baked potatoes)
;; #?="(stdin)":323:(car lset)
;; #?-    (3 diet hamburgers)
;; ()

(intersectall '((3 mangoes and)
                ()
                (3 diet hamburgers)))
;; #?="(stdin)":323:(car lset)
;; #?-    (3 mangoes and)
;; #?="(stdin)":323:(car lset)
;; #?-    ()
;; ()

; all in one ?
(define intersectall
  (lambda (lset)
    (let/cc hop
           (letrec
               ((A (lambda (lset)
                     (cond
                      ((null? (car lset))
                       (hop (quote ())))
                      ((null? (cdr lset))
                       (car lset))
                      (else (I (car lset)
                               (A (cdr lset)))))))
                (I (lambda (s1 s2)
                     (letrec
                         ((J (lambda (s1)
                               (cond
                                ((null? s1)(quote ()))
                                ((member? (car s1) s2)
                                 (cons (car s1)
                                            (J (cdr s1))))
                                (else (J (cdr s1)))))))
                       (cond
                        ((null? s2)(hop (quote ())))
                        (else (J s1)))))))
             (cond
              ((null? lset)(hop (quote ())))
              (else (A lset)))))))

(intersectall '((3 mangoes and)
                ()
                (3 diet hamburgers)))

(intersectall '((3 mangoes and)
                (3 kiwis and)
                (3 hamburgers)))


; rember


(use srfi-1)

; fold
(define (multirember a lat)
  (fold (lambda (e acc)
          (if (eq? e a)
              acc
              (cons e acc)))
        '()
        lat))

(multirember 'a '(a b c a b c))
; -> (c b c b)

; The Little Schemer
(define rember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? a (car lat))(cdr lat))
          (else (cons (car lat)
                      (rember a (cdr lat)))))))

(rember 'a '(a b c a b c))
; -> (b c a b c)

; The Seasoned Schemer
(define rember
  (letrec
      ((R (lambda (lat)
            (cond ((null? lat) '())
                  ((eq? a (car lat))(cdr lat))
                  (else (cons (car lat)
                              (rember (cdr lat))))))))
    (R lat)))

(rember 'a '(a b c a b c))


; rember-beyond-first

; (rember-beyond-first 'roots '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice))
; -> (noodles spaghetti spatzle bean-thread)

; fold
(define (rember-beyond-first a lat)
  (reverse
   (let/cc hop
     (fold (lambda (e acc)
             (if (eq? e a)
                 (hop acc)
                 (cons e acc)))
           '()
           lat))))

(rember-beyond-first 'roots '(noodles
                              spaghetti spatzle bean-thread
                              roots
                              potatoes yam
                              others rice))
; -> (noodles spaghetti spatzle bean-thread)

(rember-beyond-first 'others '(noodles
                               spaghetti spatzle bean-thread
                               roots
                               potatoes yam
                               others
                               rice))
; -> (noodles spaghetti spatzle bean-thread roots potatoes yam)

(rember-beyond-first 'sweetthing '(noodles
                               spaghetti spatzle bean-thread
                               roots
                               potatoes yam
                               others
                               rice))
; -> (noodles spaghetti spatzle bean-thread roots potatoes yam others rice)

; The Seasoned Schemer
(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat)(quote ()))
               ((eq? (car lat) a)
                (quote ()))
               (else (cons (car lat)
                           (R (cdr lat))))))))
      (R lat))))

(rember-beyond-first 'roots '(noodles
                              spaghetti spatzle bean-thread
                              roots
                              potatoes yam
                              others rice))
; -> (noodles spaghetti spatzle bean-thread)

(rember-beyond-first 'others '(noodles
                               spaghetti spatzle bean-thread
                               roots
                               potatoes yam
                               others
                               rice))
; -> (noodles spaghetti spatzle bean-thread roots potatoes yam)

(rember-beyond-first 'sweetthing '(noodles
                               spaghetti spatzle bean-thread
                               roots
                               potatoes yam
                               others
                               rice))
; -> (noodles spaghetti spatzle bean-thread roots potatoes yam others rice)



; rember-up-to-last

