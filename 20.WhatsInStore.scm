;; The Seasoned Schemer
;; chapter 20
;; What's in Store ?

(define abort '())

(define global-table '())

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

(define (atom? a)
  (and (not (pair? a))
       (not (null? a))))

(define (text-of x)
  (cadr x))

(define (formals-of x)
  (cadr x))

(define (body-of x)
  (cddr x))

(define (ccbody-of x)
  (cddr x))

(define (name-of x)
  (cadr x))

(define (right-side-of x)
  (if (null? (cddr x))
      0
      (caddr x)))

(define (cond-lines-of x)
  (cdr x))

(define (else? x)
  (if (atom? x)
      (eq? x 'else)
      #f))

(define (question-of x)
  (car x))

(define (answer-of x)
  (cadr x))

(define (function-of x)
  (car x))

(define (arguments-of x)
  (cdr x))


(define (lookup table name)
  (table name))

(define (extend name1 val table)
  (lambda (name2)
    (if (eq? name1 name2)
        val
        (table name2))))

(define (define? e)
  (eq? (and (pair? e)
            (car e)) 'define))

(define (*define e)
  (set! global-table
        (extend (name-of e)
                (box (the-meaning (right-side-of e)))
                global-table)))

(define (box it)
  (lambda (sel)
    (sel it (lambda (new)
              (set! it new)))))

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
  (print "*set")
  (setbox
   (lookup table (name-of e))
   (meaning (right-side-of e) table)))

(define (*lambda e table)
  (lambda (args)
    (beglis (body-of e)
            (multi-extend (formals-of e)
                          (box-all args)
                          table))))

(define (beglis es table)
  (cond
   ((null? (cdr es))
    (meaning (car es) table))
   (else ((lambda (val)
            (beglis (cdr es) table))
          (meaning (car es) table)))))

(define (box-all vals)
  (cond
   ((null? vals) '())
   (else (cons (box (car vals))
               (box-all (cdr vals))))))

(define (multi-extend names vals table)
  (if (null? names)
      table
      (extend (car names)(car vals)
              (multi-extend (cdr names)(cdr vals)
                            table))))

(define (*application e table)
  ((meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (evlis args table)
  (if (null? args)
      '()
      ((lambda (val)
         (cons val
               (evlis (cdr args) table)))
       (meaning (car args) table))))

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
            (extend (name-of e)
                    (box (a-prim skip) table)))))

(define (value e)
  (let/cc the-end
    (set! abort the-end)
    (if (define? e)
        (*define e)
        (the-meaning e))))

(define (the-empty-table name)
  (abort
   (cons 'no-answer
         (cons name '()))))

(define (expression-to-action e)
  (if (atom? e)
      (atom-to-action e)
      (list-to-action e)))

(define (atom-to-action e)
  (cond
   ((number? e) *const)
   ((eq? e #t) *const)
   ((eq? e #f) *const)
   ((eq? e 'cons) *const)
   ((eq? e 'car) *const)
   ((eq? e 'cdr) *const)
   ((eq? e 'null?) *const)
   ((eq? e 'eq?) *const)
   ((eq? e 'atom?) *const)
   ((eq? e 'zero?) *const)
   ((eq? e 'add1) *const)
   ((eq? e 'sub1) *const)
   ((eq? e 'number?) *const)
   (else *identifier)))

(define (list-to-action e)
  (let ((a (car e)))
    (if (atom? a)
        (let ((prim-of? (cut eq? a <>)))
          (cond 
           ((prim-of? 'quote) *quote)
           ((prim-of? 'lambda) *lambda)
           ((prim-of? 'letcc) *letcc)
           ((prim-of? 'set!) *set)
           ((prim-of? 'cond) *cond)
           (else *application)))
        *application)))


(set! global-table (lambda (name)
                     (the-empty-table name)))


(value '(sub1 1))
;; (sub1 1)
;; (1)
;; ()
;; 0

(value '(add1 1))
;; (add1 1)
;; (1)
;; ()
;; 2

(value '(define x 3))
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

(value '(define odd?
          (lambda (n)
            (cond
             ((zero? n) #f)
             (else (even? (sub1 n)))))))

(value '(define even?
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



