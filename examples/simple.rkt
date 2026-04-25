#lang effect/racket


(effect increment ())


(define (limited? _)
  (< (increment) 2))

(define/contract (f x)
    (-> limited? any)
    x)

(define (increment-service n)
    (contract-handler
      [(increment) (values n (increment-service (add1 n)))]))

(with ((increment-service 0))
    (f 1)
    (f 1))

(with ((increment-service 0))
    (f 1)
    (f 1)
    (f 1))
