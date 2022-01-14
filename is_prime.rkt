#lang racket

(define (check_prime it x)
  (cond
    [(>= it x) #t]
    [(zero? (remainder x it)) #f]
    [else (check_prime (+ it 1) x)]
    ))

(define (is_prime x)
  (check_prime 2 x)) 