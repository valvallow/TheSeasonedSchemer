(define-syntax tss-lisp-eval
  (syntax-rules ()
    ((_ expr)
     (value expr))))

(define-syntax multi-tss-lisp-eval
  (syntax-rules ()
    ((_ expr)
     (tss-lisp-eval expr))
    ((_ expr1 expr2 ...)
     (begin (tss-lisp-eval expr1)
            (multi-tss-lisp-eval expr2 ...)))))

