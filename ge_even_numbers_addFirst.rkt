#lang racket

(define (ev n)
  (cond
    [(zero? n) '()]
    [else (cons (* n 2) (ev (- n 1)))]
    ))