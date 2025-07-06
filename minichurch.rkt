#lang racket

;; A little implementation of Church (Goodman et al, 2012), inspired by Kiselyov & Shan (2009).

;; We treat distributions as hashes mapping hypotheses in the support to probabilities.

(require racket/hash)

(struct hyp (v p) #:transparent)

(define (distr/scale by d)
  (hash-map/copy d (lambda (k p) (values k (* by p)))))

(define (distr/sum d)
  (apply + (hash-values d)))

(define (distr/norm d)
  (let ((Z (distr/sum d)))
    (if (= Z 0) d (distr/scale (/ 1 Z) d))))

(define (distr/->list d)
  (map (lambda (k) (hyp (car k) (cdr k))) (hash->list d)))

(define (cumsum lyst [accum 0])
  (if (empty? lyst) '()
      (cons accum (cumsum (cdr lyst) (+ accum (car lyst))))))

(define (distr/sample! d)
  (let* ([r (random)]
         [dd (distr/->list d)]
         [cs (map cons (map hyp-v dd) (cumsum (map hyp-p dd)))]
         [flt (filter (lambda (z) (<= (cdr z) r)) cs)])
    (car (last flt))))

(define (distr/dirac x)
  (hash x 1.0))

(define distr/fail (hash))

(define (distr/uniform lyst)
  (let ([p (/ 1.0 (length lyst))])
    (make-hash (map (lambda (e) (cons e p)) lyst))))

(define (distr/merge* ds)
  (hash-filter-values (apply hash-union (hash) ds #:combine +) (negate zero?)))


;; The heart of Church: using delimited continuations to rollout all possible
;; executions when a random variable is sampled.
(require racket/control)

(define (sample d)
  (shift k (distr/merge* (for/list ([(h p) d]) (distr/scale p (k h))))))

(define-syntax-rule (condition q)
  (shift k (if q (k) distr/fail)))

(define-syntax-rule (infer x)
  (distr/norm (reset (distr/dirac x))))

;; Utilities
(define uniform-draw (compose sample distr/uniform))
(define (uniform-draw! l) ((uniform-draw l))) ;; draw from list of functions and call immediately

;; Print the top-k hypotheses in a distribution
(define (distr/top! k d)
  (define belief-to-display (sort (distr/->list d) > #:key hyp-p))
  (for ([hyp (take belief-to-display (min (length belief-to-display) k))])
    (display "P( ")
    (display (hyp-v hyp))
    (display " ) = ")
    (displayln (hyp-p hyp))
    ))


(provide (except-out (all-defined-out) cumsum))
