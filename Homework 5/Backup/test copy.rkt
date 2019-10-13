#lang racket
(define (integer-compare x y) (if (equal? x y) x (list 'if '% x y)))

(define (boolean-compare x y) (if x (if y #t '%) (if y (list 'not '%) #f)))

(define (list-compare x y) cons x y)

(define (expr-compare x y) (if (and (number? x) (number? y)) (integer-compare x y)
                                   (if (and (boolean? x) (boolean? y)) (boolean-compare x y)
                                       (if (and (list? x) (list? y) (equal? (length x) (length y))) (list-compare x y)
                                           (list 'if '% x y)))))