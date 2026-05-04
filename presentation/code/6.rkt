#lang racket

(require effect-racket)

(effect do-print (msg))
(effect do-read ())
(effect do-process (value))

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

;; must read correctly, change data to a number
(define/contract (file-read count)
  (and/c
    (-> number? string?)
    ;(->e any/c read-correct)
    (dependent->e any/c
                  (match-lambda
                    [(do-read) read-correct]
                    [else any/c]))
    (->e no-print-allow any/c))
  (string-join (map (λ (_)
                      (let ([str (do-read)]) (do-process str)))
                    (range count)) "-"))

(define (perform-read-with-data f data)
  (with
    ((file-read-like data)
     (print-handler)
     (use-processor))
    (f)))

(perform-read-with-data
  (lambda () (file-read 4))
  ;'("a" 0 "c" "d"))
  '("a" "b" "c" "d"))
