#lang racket

(require effect-racket)

(effect remaining-reads ())
(effect do-read ())

(define (file-read-like data)
  (handler
    [(do-read)
     (with ((file-read-like (cdr data)))
           (continue (car data)))]))

(define (has-reads-left eff)
  (cond
    [(do-read? eff)
     #true
     (> (remaining-reads) 0)]
    [else #true]))

(define (is-text item)
  (string? item))

(define/contract (file-read)
  (->e has-reads-left is-text)
  (do-read)
  (do-read)
  (do-read))

(define (contract-check n)
  (contract-handler
    [(remaining-reads) (values n (contract-check (- n 1)))]))

;; the main function just read, but there is a check
;; that it cannot perform more reads than possible
(define (perform-read-with-data f data)
  (with
    ;; I think there's a bug here that the effect is being
    ;; called twice, therefore we have to double the effect count
    ((contract-check (* 2 (length data)))
     (file-read-like data))
    (f)))

(perform-read-with-data
  file-read
  '("a" "b"))
