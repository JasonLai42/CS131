#lang racket
; For ints, floats, strings, etc, two arguments that aren't the same type, and lists of different sizes
(define (simple-compare x y) (if (equal? x y) x (list 'if '% x y)))

; Special case for booleans
(define (boolean-compare x y) (if x (if y #t '%) (if y (list 'not '%) #f)))

; Special case for quote datum; process element after quote as a
;(define (quote-compare x y) 

; Special case for lambda; need binding if they have different identifiers
;(define (lambda-compare x y) 

; Special case for if, process two ifs as list
; If only one is an if, process both as single elements
;(define (conditional-compare x y)


(define (list-compare x y) cons x y)

(define (aux-list x y) (if (or (equal? x '()) (equal? y '())) '()
                           (cons (expr-compare (car x) (car y)) (aux-list (cdr x) (cdr y)))))

; Only handle lambda binding, TA said let bindings don't need to be handled

(define (expr-compare x y) (if (and (boolean? x) (boolean? y)) (boolean-compare x y)
                               (if (and (list? x) (list? y) (equal? (length x) (length y))) (list-compare x y)
                                   (simple-compare x y))))