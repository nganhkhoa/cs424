#lang racket

(require effect-racket)

(effect do-print (msg))

(define (print-handler)
  (handler
    [(do-print msg)
     (printf "~a~n" msg)
     (continue null)]))

(with ((print-handler))
  (do-print "starts")

  (do-print "ends"))
