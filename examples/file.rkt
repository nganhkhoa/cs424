#lang racket

(require effect-racket)

(effect remaining-reads ())
(effect decrease-remaining-reads ())
(effect do-read ())

(define (file-read-like data)
  (handler
    [(do-read)
     (with ((file-read-like (cdr data)))
           (continue* (car data)))]))

(define (has-reads-left eff)
  (cond
    [(do-read? eff)
     (> (remaining-reads) 0)]
    [else #true]))

(define (is-eof s)
  (string=? s "\0"))

(define (reads-result-correct item)
  (let ([remainings (decrease-remaining-reads)])
    (cond
      [(equal? remainings 0) (is-eof item)]
      [else (string? item)])))

(define/contract (file-read)
  (and/c
    (-> string?)
    (->e has-reads-left reads-result-correct))
  (do-read)
  (do-read)
  (do-read)
  (do-read))

(define (contract-check n)
  (contract-handler
    [(decrease-remaining-reads) (values (- n 1) (contract-check (- n 1)))]
    [(remaining-reads) (values n (contract-check n))]))

(define (perform-read-with-data f data)
  (with
    ((contract-check (length data))
     (file-read-like data))
    (f)))

(perform-read-with-data
  file-read
  '("a" "b" "c" "\0"))
