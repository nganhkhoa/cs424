#lang racket

(provide (all-defined-out))

(require "base.rkt")
(require "meta.rkt")
(require "reduction.rkt")
(require "eval.rkt")

(require redex/reduction-semantics)

(define-metafunction effects
  ℰ : e -> e
  [(ℰ b) b]
  [(ℰ x) x]
  [(ℰ (λ (x) e)) (λ (x) (ℰ e))]
  [(ℰ (if e_1 e_2 e_3)) (if (ℰ e_1) (ℰ e_2) (ℰ e_3))]
  [(ℰ (tuple e_1 e_2)) (tuple (ℰ e_1) (ℰ e_2))]
  [(ℰ (e_1 e_2)) ((ℰ e_1) (ℰ e_2))]
  [(ℰ (o e)) (o (ℰ e))]
  [(ℰ (do e)) (do (ℰ e))]

  ;; main contract erase normally
  [(ℰ (handle ▷ e with e_h)) (handle ▷ (ℰ e) with (ℰ e_h))]

  [(ℰ (mon (k l j) e_1 e_2)) (ℰ e_2)]
  [(ℰ (handle ♢ e with e_h)) (handle ♢ (ℰ e) with (ℰ e_h))])

(define-metafunction effects
  ℰ+ : e -> e
  [(ℰ+ b) b]
  [(ℰ+ x) x]
  [(ℰ+ (λ (x) e)) (λ (x) (ℰ+ e))]
  [(ℰ+ (if e_1 e_2 e_3)) (if (ℰ+ e_1) (ℰ+ e_2) (ℰ+ e_3))]
  [(ℰ+ (tuple e_1 e_2)) (tuple (ℰ+ e_1) (ℰ+ e_2))]
  [(ℰ+ (e_1 e_2)) ((ℰ+ e_1) (ℰ+ e_2))]
  [(ℰ+ (o e)) (o (ℰ+ e))]
  [(ℰ+ (do e)) (do (ℰ+ e))]

  ;; main contract erase normally
  [(ℰ+ (handle ▷ e with e_h)) (handle ▷ (ℰ+ e) with (ℰ+ e_h))]

  [(ℰ+ (mon (k l j) e_1 e_2)) (ℰ+ e_2)]

  ;; this is handler for contract, it can be removed
  [(ℰ+ (handle ♢ e with e_h)) (ℰ+ e)])


(module+ test
  (test-equal (term (ℰ (mon (k l j) false (tuple true false))))
              (term (tuple true false)))

  (test-equal (term (ℰ+ (mon (k l j) false (tuple true false))))
              (term (tuple true false)))

  (test-equal (term (ℰ+ (handle ♢ (tuple true false) with (tuple (λ (x) x) (λ (y) y)))))
              (term (tuple true false)))
)
