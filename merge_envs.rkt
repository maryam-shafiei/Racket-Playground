#lang racket

(define find (lambda (x lst)
               (cond
                 [(equal? lst null) null]
                 [(equal? (car x) (car (car lst))) x]
                 [else (find x (cdr lst))]
  )))

(define (merge_envs E1 E2)
  (cond
    [(equal? E1 null) E2]
    [(equal? (find (car E1) E2) null) (cons (car E1) E2)]
    [else (merge_envs (cdr E1) E2)]
    ))
