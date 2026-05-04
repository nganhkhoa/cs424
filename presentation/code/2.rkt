#lang racket

(require effect/racket)

(effect get ())
(effect set (x))

(define (state-manager s)
  (handler
    [(get) (continue s)]
    [(set x) (with ((state-manager x)) (continue* x))]))

(with ((state-manager 0))
      (set 10)
      (get))
