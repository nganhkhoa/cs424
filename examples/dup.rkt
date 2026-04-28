#lang racket

(require effect-racket)

(effect dummy ())

;; is this a bug? check-effect is being called twice?
(define (check-effect eff)
  (println "checking effect")
  #true)

(define/contract (f)
  (->e check-effect any/c)
  (dummy))

(with ((handler
         [(dummy) (continue 1)]
         #;[(return v) v]))
      (f))
