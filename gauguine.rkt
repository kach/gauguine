#lang racket

(require json)

; https://docs.racket-lang.org/guide/eval.html#%28part._.Namespaces_and_.Modules%29
(define-namespace-anchor *nsa*)
(define ns (namespace-anchor->namespace *nsa*))

(require "minichurch.rkt")

;; The following defines `evaluate`, in a way that is robust to various kinds
;; of errors that might occur, and is also memoized for performance.
(define *evaluate-cache* (make-hash))
(define *evaluate-lock* (make-hash))
(define (evaluate program . arguments)
  (with-handlers ([exn:fail? (lambda (exn) distr/fail)])
    (let ([key (cons program arguments)])
      (when (hash-has-key? *evaluate-lock* key)
        (error 'infinite-loop))  ;; catches some runaway programs
      (unless (hash-has-key? *evaluate-cache* key)
        (hash-set! *evaluate-lock* key #t)
        (hash-set! *evaluate-cache* key (apply (eval program ns) arguments))
        (hash-remove! *evaluate-lock* key))
      (hash-ref *evaluate-cache* key))))

;; Useful helper function
(define (postfixes lst)
  (if (empty? lst) '() (cons lst (postfixes (cdr lst)))))

(define (<guesser> n)
  (condition (>= n 0))
  (let ([g (<guess> (- n 1))])
    ;; For tractability: limit to programs with at most 2 instances of `sample`
    (condition (<= (count (lambda (x) (eq? x 'sample)) (flatten g)) 2))
    `(lambda (history) (infer ,g))))

(define (<guess> n)
  (condition (>= n 0))
  (uniform-draw!
    (list
      (lambda () 'g)
      (lambda () 'h)
      (lambda () `(car ,(<history> 0)))
      (lambda () `(sample programs))
      (lambda () `(sample (evaluate ,(<guess> (min 0 (- n 1))) ,(<history> 1))))
      (lambda ()
         (let ([v (<void> (- n 1))])
          `(let ([g (sample programs)]) ,v g)
         )
        )
      )))

(define (<void> n)
  (condition (>= n 0))
  (uniform-draw!
   (list
    (lambda () `(condition ,(<bool> (- n 1))))
    (lambda () (let ([b (<bool> (- n 1))])
      `(for ([h history]) (condition ,b))))

    (lambda () (let ([b (<bool> (- n 1))])
      `(for ([history (postfixes history)]) (condition ,b))))
        )))

(define (<history> n)
  (condition (>= n 0))
  (uniform-draw!
    (list
      (lambda () 'history)
      (lambda () '(cdr history))
      (lambda () '(list))
      )))

(define (<bool> n)
  (condition (>= n 0))
  (uniform-draw!
    (list
      (lambda () (let ([p1 (<guess> (- n 1))]
                       [p2 (<guess> (- n 1))])
                      ;; For tractability: break symmetry of `equal?`
                      (condition (>= (equal-hash-code p1) (equal-hash-code p2)))
                      `(equal? ,p1 ,p2)))
      )))

(define programs (infer (<guesser> 5)))  ;; prior distribution over programs (cached for performance)


;; The main event
(define the-gauguine
  '(lambda (history)
    (infer
     (let ([g (sample programs)])
      (for ([history (postfixes history)])
       (condition (equal? (sample (evaluate g (cdr history))) (car history))))
      g))))

(unless (hash-has-key? programs the-gauguine)
  (displayln "WARNING: the gauguine is NOT in its own hypothesis space!"))

(define (main!)
  (define history '())
  (for ([i (in-range 25)])
    (let* ([belief (evaluate the-gauguine history)]
           [guess (distr/sample! belief)])
      (display "** Generation ")
      (displayln i)

      (display "Guess: ")
      (displayln guess)

      (set! history (cons guess history))

;     Output JSON for creating figures in paper...
;     (write-json (hash-map/copy belief (lambda (h p) (values (string->symbol (pretty-format h)) p))))

      (displayln "Top 3 hypotheses:")
      (distr/top! 3 belief)
      (newline)
      (flush-output)
      )))

(main!)
