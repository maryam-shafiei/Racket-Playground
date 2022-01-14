#lang racket

(define (append_end lst x)
  (cond
    [(null? lst) (cons x null)]
    [else (cons (car lst) (append_end (cdr lst) x))]
    ))

(define (ev n)
  (cond
    [(zero? n) '()]
    [else (append_end (ev (- n 1)) (* n 2))]
    ))