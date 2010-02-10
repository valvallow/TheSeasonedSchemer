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

