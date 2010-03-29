;; chapter.15

(define x
  (cons (quote chicago)
        (cons (quote pizza)
              (quote ()))))
; -> x

x
; -> (chicago pizza)

(set! x (quote gone))
; -> gone

x
; -> gone


(set! x (quote skins))
; -> skins

x
; -> skins


(define gourmet
  (lambda (food)
    (cons food
          (cons x (quote ())))))

x
; -> skins

(gourmet (quote onion))
; -> (onion skins)

(set! x (quote rings))

(gourmet (quote onion))
; -> (onion rings)


(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x
                (quote ())))))

(gourmand (quote potato))
; -> (potato potato)


(define dinner
  (lambda (food)
    (cons (quote milkshake)
          (cons food
                (quote ())))))

(define dinnerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
          (cons food
                (quote ())))))

(dinnerR (quote onion))
; -> (milkshake onion)

(dinnerR (quote pecanpie))
; -> (milkshake pecanpie)

x
; -> pecanpie

(gourmand (quote onion))
; -> (onion onion)

x
; -> onion


(define omnivore
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  (quote ()))))))

(omnivore (quote bouillabaisse))
; -> (bouillabaisse bouillabaisse)


(define gobbler
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  (quote ()))))))

(gobbler (quote gumbo))
; -> (gumbo gumbo)


(define nibbler
  (lambda (food)
    (let ((x (quote donut)))
      (set! x food)
      (cons food
            (cons x
                  (quote ()))))))

(nibbler (quote cheerio))
; -> (cheerio cheerio)


;; Isn't (let ...) like (letrec ...)


(define food (quote none))

(define glutton
  (lambda (x)
    (set! food x)
    (cons (quote more)
          (cons x
                (cons (quote more)
                      (cons x
                            (quote ())))))))

(glutton (quote garlic))
; -> (more garlic more garlic)

food
; -> garlic


(define chez-nous
  (lambda ()
    (set! food x)
    (set! x food)))

food
; -> garlic

x
; -> onion

(chez-nous)

food
; -> onion

x
; -> onion


(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(glutton (quote garlic))
; -> (more garlic more garlic)

food
; -> garlic

(gourmand (quote potato))
; -> (potato potato)

x
; -> potato

(chez-nous)

food
; -> potato

x
; -> garlic

