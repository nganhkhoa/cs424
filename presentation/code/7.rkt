#lang racket

(require effect-racket)

(effect do-print (msg))
(effect do-read ())
(effect do-process (value))

(effect remaining-reads ())

(define (print-handler)
  (handler
    [(do-print msg)
     (printf "~a~n" msg)
     (continue null)]))

(define (file-read-like data)
  (handler
    [(do-read)
     (with ((file-read-like (cdr data)))
           (continue* (car data)))]))

(define (use-processor)
  (handler
    [(do-process v)
     (continue (format "(~a)" v))]))


(define (no-print-allow eff) (not (do-print? eff)))

(define (read-correct value)
  (and (string? value)
       (andmap char-alphabetic? (string->list value))))

(define (has-reads-left eff)
  (cond
    [(do-read? eff)
     (> (remaining-reads) 0)]
    [else #true]))

;; check how many reads left
;; there's a bug in the implementation
;; but the idea remains
(define/contract (file-read count)
  (and/c
    (-> number? string?)
    (dependent->e has-reads-left
                  (match-lambda
                    [(do-read) read-correct]
                    [else any/c]))
    (->e no-print-allow any/c))
  (string-join (map (λ (_)
                      (let ([str (do-read)]) (do-process str)))
                    (range count)) "-"))

(define (contract-check n)
  (contract-handler
    [(remaining-reads) (values n (contract-check (- n 1)))]))

(define (perform-read-with-data f data)
  (with
    ((contract-check (* 2 (length data)))
     (file-read-like data)
     (print-handler)
     (use-processor))
    (f)))

(perform-read-with-data
  (lambda () (file-read 4))
  '("a" "b" "c" "d"))


