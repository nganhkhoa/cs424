#lang racket

(provide (all-defined-out))

(require "base.rkt")
(require "meta.rkt")

(require redex/reduction-semantics)

;; core
(define ->core
  (reduction-relation dependent-eval
    [--> (if v e_1 e_2) e_1
         (side-condition (not (redex-match? dependent-eval false (term v))))
         IF-TRUE]
    [--> (if false e_1 e_2) e_2
         IF-FALSE]
    [--> ((λ (x) e) v) (substitute e x v)
         APP-LAMBDA]
    [--> (o v) (delta o v)
         APP-OP]))

;; contract
(define ->contract
  (extend-reduction-relation ->core dependent-eval
    [--> (mon (k l j) true v) v
         MON-TRUE]
    [--> (mon (k l j) false v) (err k j)
         MON-FALSE]
    [--> (mon (k l j) f v) (mon (k l j) (f v) v)
         MON-FLAT]
    [--> (mon (k l j) (tuple v_1 v_2) v) (err k j)
         (side-condition (not (redex-match? dependent-eval (tuple v_3 v_4) (term v))))
         MON-PAIR]
    [--> (mon (k l j) (tuple v_1 v_2) (tuple v_3 v_4)) (tuple (mon (k l j) v_1 v_3) (mon (k l j) v_2 v_4))
         GRD-PAIR]
    [--> (mon (k l j) (v_1 -> v_2) v) (err k j)
         (side-condition (not (redex-match? dependent-eval f (term v))))
         MON-FUN]
    [--> (mon (k l j) (v_1 -> v_2) f) (λ (x) (mon (k l j) v_2 (f (mon (l k j) x))))
         GRD-FUN]))

(define ->effects
  (extend-reduction-relation ->contract dependent-eval
    [--> (handle m v with v_h) v
         HANDLE]

    ;; the other rules need side-condition with some metafunction
    ;; is-unhandled collect-pre collect-post

    ;; ATTENTION:
    ;; DO▷ in Fig. 3 and Section 4 (dependent) are different
    ))

(define ->effects-eval
  (extend-reduction-relation ->effects dependent-eval
    [--> (mon (k l j) (v_1 ▷ v_2) v) (err k j)
         (side-condition (not (redex-match? dependent-eval f (term v))))
         MON-HANDLE▷]
    [--> (mon (k l j) (v_1 ▷ v_2) f) (λ (x) (mark (k l j) (v_1 ▷ v_2) (f x)))
         GRD-HANDLE▷]
    [--> (mark (k l j) v_k v) v
         MARK]
    [--> (mon (k l j) (♢ v_h) v) (err k j)
         (side-condition (not (redex-match? dependent-eval f (term v))))
         MON-HANDLE♢]
    [--> (mon (k l j) (♢ v_h) f) (λ (x) (handle ♢ (f x) with v_h))
         GRD-HANDLE♢]))

(define ->dependent-eval
  (extend-reduction-relation ->effects-eval dependent-eval
    [--> (mon (k l j) (v_1 => v_2) v) (err k j)
         (side-condition (not (redex-match? dependent-eval f (term v))))
         MON-DEP-FUN]
    [--> (mon (k l j) (v_1 => v_2) f)
         (λ (x) (mon (k l j) (v_2 (mon (l j j) v_1 x)) (f (mon (l k j) v_1 x))))
         GRD-DEP-FUN]
    [--> (mon (k l j) (v_1 ▶ v_2) v) (err k j)
         (side-condition (not (redex-match? dependent-eval f (term v))))
         MON-HANDLE▶]
    [--> (mon (k l j) (v_1 ▶ v_2) f)
         (λ (x) (mark (k l j) (v_1 ▶ v_2) (f x)))
         GRD-HANDLE▶]))

(define -> ->dependent-eval)

(define ->*
  (compatible-closure -> dependent-eval E))

(module+ test
  ;; core reduction tests
  (test-->> ->*
            (term (if false true false))
            (term false))

  (test-->> ->*
            (term ((λ (x) (tuple x true)) false))
            (term (tuple false true)))

  (test-->> ->*
            (term (fst (tuple (λ (x) true) (λ (y) false))))
            (term (λ (x) true)))

  (test-->> ->*
            (term (snd (tuple (λ (x) true) (λ (y) false))))
            (term (λ (y) false)))

  ;; contract reduction tests
  (test-->> ->*
            (term (mon (k l j) true true))
            (term true))

  (test-->> ->*
            (term (mon (k l j) false true))
            (term (err k j)))

  ;; effects reduction tests
  (test-->> ->*
         (term (handle ▷ (tuple true false) with (λ (x) x)))
         (term (tuple true false)))

  (test-->> ->*
            (term (handle ♢ (tuple true false) with (tuple (λ (x) y) false)))
            (term (tuple true false)))


  ;; effects-eval reduction tests

  ;; dependent-eval reduction tests

  )
