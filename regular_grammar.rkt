#lang racket
(struct rule (left_nonterminal terminal right_nonterminal) #:transparent)

(define (indx nt rules)
  (cond
    [(equal? rules null) '()]
    [(equal? nt (rule-left_nonterminal (car rules))) (cons (car rules) (indx nt (cdr rules)))]
    [else (indx nt (cdr rules))]
    ))

(define (s_rule rules)
  (indx "S" rules))
(define (a_rule rules)
  (indx "A" rules))
(define (b_rule rules)
  (indx "B" rules))
(define (c_rule rules)
  (indx "C" rules))
(define (d_rule rules)
  (indx "D" rules))

(define (detect_rnt rul rules)
  (cond
    [(equal? (rule-right_nonterminal rul) "S") (s_rule rules)]
    [(equal? (rule-right_nonterminal rul) "A") (a_rule rules)]
    [(equal? (rule-right_nonterminal rul) "B") (b_rule rules)]
    [(equal? (rule-right_nonterminal rul) "C") (c_rule rules)]
    [(equal? (rule-right_nonterminal rul) "D") (d_rule rules)]
    ))

(define (mtch x nt_rules)
  (cond
    [(equal? nt_rules null) '()]
    [(equal? x (rule-terminal (car nt_rules))) (car nt_rules)]
    [else (mtch x (cdr nt_rules))]
    ))

(define (sub_start nxt_rules str rules i)
    (cond
      [(equal? i (string-length str)) #t]
      [(equal? (mtch (substring str i (+ i 1)) nxt_rules) null) #f]
      [else (let ([nxt (detect_rnt (mtch (substring str i (+ i 1)) nxt_rules) rules)])
              (sub_start nxt str rules (+ i 1)))]
      ))
     

(define (start str rules i)
  (let ([w (mtch (substring str i (+ i 1)) (s_rule rules))])
    (cond
      [(equal? w null) #f]
      [else (let ([nxt (detect_rnt w rules)])
              (sub_start nxt str rules (+ i 1)))]
      )))

(define (is_derived gr str)
  (start str gr 0))