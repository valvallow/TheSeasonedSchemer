;; chapter 20
;; What's in Store ?

(use srfi-1)

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (atom? a)
  (and (not (pair? a))
       (not (null? a))))

;; (define (atom? a)
;;   (every (cut <> a)
;;          (map (cut compose not <>)
;;               `(,pair? ,null?))))

;; (define (atom? a)
;;   (every (cut <> a)
;;          (map complement
;;               `(,pair? ,null?))))


(define global-table '())

(define (lookup table name)
  (table name))

(define (extend name1 value table)
  (lambda (name2)
    (if (eq? name1 name2)
        value
        (table name2))))

;; (define (define? e)
;;   (cond
;;    ((atom? e) #f)
;;    ((atom? (car e))
;;     (eq? (car e)(quote define)))
;;    (else #f)))

;; (define (define? e)
;;   (let ((kar (if (atom? e)
;;                  #f
;;                  (car e))))
;;     (eq? kar 'define)))

;; (define (define? e)
;;   (eq? (if (pair? e)
;;            (car e)
;;            #f) 'define))

(define (define? e)
  (eq? (and (pair? e)
            (car e)) 'define))

(define (*define e)
  (set! global-table
        (extend
         (name-of e)
         (box
          (the-meaning
           (right-side-of e)))
         global-table)))

(define (box it)
  (lambda (sel)
    (sel it (lambda (new)
              (set! it new)))))

;; (define (box it)
;;   (lambda (sel)
;;     (sel it (cut set! it <>))))

;; (define (box it)
;;   (cut <> it (cut set! it <>)))

(define (setbox box new)
  (box (lambda (it set)
         (set new))))

(define (unbox box)
  (box (lambda (it set)
         it)))

(define (the-meaning e)
  (meaning e lookup-in-global-table))

(define (lookup-in-global-table name)
  (lookup global-table name))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*quote e table)
  (text-of e))

(define (*identifier e table)
  (unbox (lookup table e)))

(define (*set e table)
  (setbox
   (lookup table (name-of e))
   (meaning (right-side-of e) table)))

(define (*lambda e table)
  (lambda (args)
    (beglis (body-of e)
            (multi-extend
             (formals-of e)
             (box-all args)
             table))))

(define (beglis es table)
  (cond
   ((null? (cdr es))
    (meaning (car es) table))
   (else ((lambda (val)
            (beglis (cdr es) table))
          (meaning (car es) table)))))

;; (define (beglis es table)
;;   (let ((a (car es))
;;         (d (cdr es)))
;;     (if (null? d)
;;         (meaning a table)
;;         ((lambda (val)
;;            (beglis d table))
;;          (meaning a table)))))

;; (define (beglis es table)
;;   (let ((m (meaning (car es) table))
;;         (d (cdr es)))
;;     (if (null? d)
;;         m
;;         ((lambda (val)
;;            (beglis d table)) m))))

;; (define (beglis es table)
;;   (let* ((f (cut <> <> table))
;;          (m (meaning (car es)))
;;          (d (cdr es)))
;;     (if (null? d)
;;         m
;;         ((lambda (val)
;;            (f beglis d)) m))))

;; (define (beglis es table)
;;   (let ((m (meaning (car es) table))
;;         (d (cdr es)))
;;     (if (null? d)
;;         m
;;         (let ((val m))
;;           (beglis d table)))))

;; (define (beglis es table)
;;   (let ((m (meaning (car es) table))
;;         (d (cdr es)))
;;     (if (null? d)
;;         m
;;         (beglis d table))))

(define (box-all vals)
  (cond
   ((null? vals)(quote ()))
   (else (cons (box (car vals))
               (box-all (cdr vals))))))

;; (define (box-all vals)
;;   (if (null? vals)
;;       '()
;;       (cons (box (car vals))
;;             (box-all (cdr vals)))))

;; (define (box-all vals)
;;   (reverse
;;    (fold (lambda (e acc)
;;            (cons (box e) acc))
;;          '() vals)))

(define (multi-extend names values table)
  (if (null? names)
      table
      (extend (car names)(car values)
              (multi-extend (cdr names)(cdr values)
                            table))))

;; (define (multi-extend names values table)
;;   (fold-right (lambda (n v acc)
;;                 (extend n v acc))
;;               table names values))

(define (*application e table)
  ((meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (evlis args table)
  (if (null? args)
      (quote ())
      ((lambda (val)
         (cons val
               (evlis (cdr args) table)))
       (meaning (car args) table))))

;; (define (evlis args table)
;;   (if (null? args)
;;       '()
;;       (let ((val (meaning (car args) table)))
;;         (cons val
;;               (evlis (cdr args) table)))))

;; (define (evlis args table)
;;   (if (null? args)
;;       '()
;;       (cons (meaning (car args) table)
;;             (evlis (cdr args) table))))

;; (define (evlis args table)
;;   (fold-right (lambda (e acc)
;;                 (cons (meaning e table)
;;                       acc))
;;               table args))

;; :car
(define (ccar args-in-a-list)
  (caar args-in-a-list))

(define (a-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list))))

(define (b-prim p)
  (lambda (args-in-a-list)
    (p (car args-in-a-list)
       (cadr args-in-a-list))))

(define (*const e table)
  (cond
   ((number? e) e)
   ((eq? e #t) #t)
   ((eq? e #f) #f)
   ((eq? e 'cons)(b-prim cons))
   ((eq? e 'car )(a-prim car))
   ((eq? e 'cdr)(a-prim cdr))
   ((eq? e 'eq?)(b-prim eq?))
   ((eq? e 'atom?)(a-prim atom?))
   ((eq? e 'null?)(a-prim null?))
   ((eq? e 'zero?)(a-prim zero?))
   ((eq? e 'add1)(a-prim add1))
   ((eq? e 'sub1)(a-prim sub1))
   ((eq? e 'number)(a-prim number?))))


;; (define (*const e table)
;;   (let ((q (cut eq? e <>)))
;;     (cond
;;      ((number? e) e)
;;      ((q #t) #t)
;;      ((q #f) #f)
;;      ((q 'cons)(b-prim cons))
;;      ((q 'car )(a-prim car))
;;      ((q 'cdr)(a-prim cdr))
;;      ((q 'eq?)(b-prim eq?))
;;      ((q 'atom?)(a-prim atom?))
;;      ((q 'null?)(a-prim null?))
;;      ((q 'zero?)(a-prim zero?))
;;      ((q 'add1)(a-prim add1))
;;      ((q 'sub1)(a-prim sub1))
;;      ((q 'number)(a-prim number?)))))

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define (evcon lines table)
  (cond
   ((else? (question-of (car lines)))
    (meaning (answer-of (car lines)) table))
   ((meaning (question-of (car lines)) table)
    (meaning (answer-of (car lines)) table))
   (else (evcon (cdr lines) table))))


(define (*letcc e table)
  (let/cc skip
    (beglis (ccbody-of e)
            (extend
             (name-of e)
             (box (a-prim skip) table)))))

(define (value e)
  (let/cc the-end
    (set! abort the-end)
    (cond
     ((define? e)(*define e))
     (else (the-meaning e)))))

(define (the-empty-table name)
  (abort
   (cons (quote no-answer)
         (cons name (quote ())))))

(define (expression-to-action e)
  (cond
   ((atom? e)(atom-to-action e))
   (else (list-to-action e))))

(define (atom-to-action e)
  (cond
   ((number? e) *const)
   ((eq? e #t) *const)
   ((eq? e #f) *const)
   ((eq? e (quote cons)) *const)
   ((eq? e (quote car)) *const)
   ((eq? e (quote cdr)) *const)
   ((eq? e (quote null?)) *const)
   ((eq? e (quote eq?)) *const)
   ((eq? e (quote atom?)) *const)
   ((eq? e (quote zero?)) *const)
   ((eq? e (quote add1)) *const)
   ((eq? e (quote sub1)) *const)
   ((eq? e (quote number?)) *const)
   (else *identifier)))

(define (list-to-action e)
  (cond
   ((atom? (car e))
    (cond 
     ((eq? (car e)(quote quote)) *quote)
     ((eq? (car e)(quote lambda)) *lambda)
     ((eq? (car e)(quote letcc)) *letcc)
     ((eq? (car e)(quote set!)) *set)
     ((eq? (car e)(quote cond)) *cond)
     (else *application)))
   (else *application)))

(define (text-of x)
  (car (cdr x)))

(define (formals-of x)
  (car (cdr x)))

(define (body-of x)
  (cdr (cdr x)))

(define (ccbody-of x)
  (cdr (cdr x)))

(define (name-of x)
  (car (cdr x)))

(define (right-side-of x)
  (cond
   ((null? (cdr (cdr x))) 0)
   (else (car (cdr (cdr x))))))

(define (cond-lines-of x)
  (cdr x))

(define (else? x)
  (cond
   ((atom? x)(eq? x (quote else)))
   (else #f)))

(define (question-of x)
  (car x))

(define (answer-of x)
  (car (cdr x)))

(define (function-of x)
  (car x))

(define (arguments-of x)
  (car x))

(define abort '())


(value '(define value
          (lambda (e)
            (letcc the-end
                   (set! abort the-end)
                   (cond
                    ((define? e)(*define e))
                    (else (the-meaning e)))))))


(value '(define x 3))
(value 'x)
; -> 3

(value '(define y x))
(value 'y)
; -> 3

(value '(set! x 5))
(value 'x)
; -> 5

(value 'y)
; -> 3

(value 'car)

(value 1)

