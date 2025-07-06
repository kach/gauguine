#lang racket

;; These are some warm-ups to practice using Church to perform probabilistic
;; inference over probabilistic programs.

(require "minichurch.rkt")

(displayln "Example from Sec 2")
(define die (distr/uniform '(1 2 3 4 5 6)))
(define fair-coin (distr/uniform '(0 1)))
(define (toss k)
  (if (= k 0) '() (cons (sample fair-coin)
                        (toss (- k 1)))))
(distr/top! 3
  (infer
    (let* ((k (sample die))
           (sequence (toss k)))
          (condition (= (apply + sequence) 3))
          sequence)))



(newline)
(displayln "Example from Sec 2.1")

(define suits '(clubs diamonds hearts spades))
(define ranks '(A 2 3 4 5 6 7 8 9 10 J Q K))
(define cards (cartesian-product ranks suits))

(define (face? c) (member (car c) '(J Q K)))
(define (numbered? c) (negate face?))
(define (clubs? c) (equal? (cadr c) 'clubs))
(define (diamonds? c) (equal? (cadr c) 'diamonds))
(define (hearts? c) (equal? (cadr c) 'hearts))
(define (spades? c) (equal? (cadr c) 'spades))

(define primitives
  (distr/uniform '(clubs? diamonds? hearts? spades? face? numbered?)))

(define (<pred> d)  ; samples a predicate
  (condition (> d 0))
  ((sample
    (distr/uniform
    (list
      (lambda () `(not ,(<pred> (- d 1))))
      (lambda () `(and ,(<pred> (- d 1))
                       ,(<pred> (- d 1))))
      (lambda () `(or  ,(<pred> (- d 1))
                       ,(<pred> (- d 1))))
      (lambda () `(,(sample primitives) x))
      )))))

(define-namespace-anchor *nsa*)
(define ns (namespace-anchor->namespace *nsa*))
(define (evaluate x) (eval x ns))

(distr/top! 3
  (infer
    (let ((phi (<pred> 2)))
      (condition
       (and
        (evaluate `(let ((x '(K hearts))) ,phi))
        (evaluate `(let ((x '(J hearts))) ,phi))
        (evaluate `(let ((x '(2 spades))) (not ,phi)))
        (evaluate `(let ((x '(3 diamonds))) (not ,phi)))
        ))
      phi)))


(newline)
(displayln "Example from Sec 2.2")
(define (make-distr phi)
  (evaluate `(infer
           (let ((x (sample (distr/uniform cards))))
                 (condition ,phi)
                 x))))

(distr/top! 3
  (infer
    (let* ((phi (<pred> 2))
           (D (make-distr phi)))
         (condition
           (and
             (equal? '(K clubs) (sample D))
             (equal? '(Q clubs) (sample D))
             (equal? '(Q clubs) (sample D))
             (equal? '(J clubs) (sample D))
             ))
         phi)))
