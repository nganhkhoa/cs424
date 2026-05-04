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

;; original program
(define/contract (file-read count)
  (and/c
    (-> number? string?))
  (string-join (map (λ (_)
                      (let ([str (do-read)])
                        (do-print (format "reading secrets: ~a" str))
                        (do-process str)))
                    (range count)) "-"))

(define (perform-read-with-data f data)
  (with
    ((file-read-like data)
     (print-handler)
     (use-processor))
    (f)))

(perform-read-with-data
  (lambda () (file-read 3))
  '("a" "b" "c" 0))
