#lang racket

(require effect-racket)

(effect remaining-reads ())
(effect do-read ())

(define (file-read-like data)
  (handler
    [(do-read)
     (unless (pair? data)
       (error 'file-read-like "out of data"))
     (with ((file-read-like (cdr data)))
       (continue* (car data)))]))

(define (has-reads-left eff)
  (cond
    [(do-read? eff)
     (> (remaining-reads) 0)]
    [else #t]))

(define (is-text item)
  (string? item))

(define/contract (file-read)
  (->e has-reads-left is-text)
  (do-read)
  (do-read)
  (do-read))

(define (contract-check n)
  (contract-handler
    [(remaining-reads)
     (values n (contract-check (sub1 n)))]))

;; the main function just read, but there is a check
;; that it cannot perform more reads than possible
(define (perform-read-with-data f data)
  (with ((contract-check (length data))
         (file-read-like data))
    (f)))

(perform-read-with-data
 file-read
 '("a" "b"))
