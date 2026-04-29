#lang racket

(require effect-racket)

(effect do-print (msg))
(effect remaining-reads ())
(effect decrease-remaining-reads ())
(effect do-read ())

(define (file-read-like data)
  (handler
    [(do-read)
     (with ((file-read-like (cdr data)))
           (continue* (car data)))]))

(define (print-handler)
  (handler
    [(do-print msg)
     (printf "~a~n" msg)
     (continue null)]))

(define (has-reads-left eff)
  (cond
    [(do-read? eff)
     (> (remaining-reads) 0)]
    [else #true]))

(define (is-eof s) (equal? s 0))

(define (reads-result-correct item)
  (let ([remainings (decrease-remaining-reads)])
   (cond
     [(equal? remainings 0) (is-eof item)]
     [else (string? item)])))

(define (no-print-allow eff) (not (do-print? eff)))

(define/contract (file-read count)
  (and/c
    (-> number? string?)
    (dependent->e has-reads-left
                  (match-lambda
                    [(do-read) reads-result-correct]
                    [else any/c]))
    (->e no-print-allow any/c))
  (string-join (map (λ (_)
                      (let ([str (do-read)])
                        ;; (do-print (string-append "reading: " str))
                        str))
                    (range count)) "-"))

(define (contract-check n)
  (contract-handler
    [(decrease-remaining-reads) (values (- n 1) (contract-check (- n 1)))]
    [(remaining-reads) (values n (contract-check n))]))

(define (perform-read-with-data f data)
  (with
    ((contract-check (length data))
     (file-read-like data))
    (f)))

(with ((print-handler))
  (do-print "starts")
  (perform-read-with-data
    (λ () (file-read (read)))
    '("a" "b" "c" 0))
  (do-print "ends"))
