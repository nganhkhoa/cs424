;; these example are for main-effect contract

#lang effect/racket

(effect get ())
(effect set (x))

(define (state-manager s)
  (handler
    [(get) (continue s)]
    [(set x) (with ((state-manager x)) (continue* x))]))

;; example: state-manager works
;; (with ((state-manager 0))
;;       (let ([x (set 1)])
;;         (get)))

;; wrapper over anything that needs a state
(define (run-with-state f)
  (with ((state-manager 0))
        (f)))

;; example: run-with-state works
(define/contract (free-to-do-anything)
  (-> any/c)
  (+ (get) (set 100) (get)))
(run-with-state free-to-do-anything)

;; allow all effects, except set
(define no-set/c
  (->e (lambda (x) (not (set? x))) any/c))

;; directly call set inside
(define/contract (no-set-allow-1)
  no-set/c
  (+ (get)
     ;; (set 1) ;; not allowed
     (get)))
;; (run-with-state no-set-allow-1)

(define (call-set-here)
  (set 10))

;; indirectly call set inside
(define/contract (no-set-allow-2)
  no-set/c
  (+ (get)
     ;; (call-set-here) ;; not allowed
     (get)))
;; (run-with-state no-set-allow-2)

(define only-set-lower-10/c
  (->e any/c (lambda (x) (< x 10))))

(define/contract (set-something-small)
  only-set-lower-10/c
  (set 9))
(run-with-state set-something-small)

(define/contract (set-something-big)
  only-set-lower-10/c
  (set 11))
;; (run-with-state set-something-big)
