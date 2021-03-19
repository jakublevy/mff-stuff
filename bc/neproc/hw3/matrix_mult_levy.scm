; testovano v guile 2.2.4 a racketu v7.2

; na Windowsu racket vyzadoval odkomentovat nasledujici radek
; #lang racket/base

(define (vec_vec_mult a b)
    (if (null? a)
        0
        (+ (* (car a) (car b)) (vec_vec_mult (cdr a) (cdr b)) )
    )
)

(define (transpose A)
    (if (null? (car A))
        '()
        (cons (map (lambda (x) (car x)) A) (transpose (map (lambda (x) (cdr x)) A)))
    )
)

(define (vec_matrix_mult vec A)
    (if (null? A)
        '()
        (cons (vec_vec_mult (car A) vec) (vec_matrix_mult vec (cdr A)))
    )
)

(define (matrix_matrix_mult A BT)
    (if (null? A)
        '()
        (cons (vec_matrix_mult (car A) BT) (matrix_matrix_mult (cdr A) BT))
    )
)

(define (mult-mtx B . args)
    (define A B)
    (for-each
        (lambda (C) (set! A (matrix_matrix_mult A (transpose C))))
    args)
    A
)