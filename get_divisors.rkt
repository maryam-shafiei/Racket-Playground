#lang racket

(define (itr_get_div it x)
  (cond
    [(> it x) '()]
    [else (let ([other (itr_get_div (+ it 1) x)])(
     if (zero? (remainder x it)) (cons it other) other))]
    ))

(define (get_div x)
  (itr_get_div 1 x))
