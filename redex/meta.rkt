#lang racket

(provide (all-defined-out))

(require "base.rkt")

(require redex/reduction-semantics)

;; this file provides metafunctions for reduction rules

(define-metafunction dependent-eval
  delta : o v -> v
  [(delta fst (tuple v_1 v_2)) v_1]
  [(delta snd (tuple v_1 v_2)) v_2])

(define-metafunction dependent-eval
  unhandled : m any -> boolean
  [(unhandled ▷ (in-hole E_2 (handle ▷ E▷_3 with v))) #false]
  [(unhandled ♢ (in-hole E_2 (handle ♢ E♢_3 with v))) #false]
  [(unhandled _ _) #true])

;; groups all marks pre-condition
(define-metafunction dependent-eval
  ↑ : any -> any
  [(↑ hole) hole]
  [(↑ (tuple E e)) (↑ E)]
  [(↑ (tuple v E)) (↑ E)]
  [(↑ (if E e_1 e_2)) (↑ E)]
  [(↑ (E e)) (↑ E)]
  [(↑ (v E)) (↑ E)]
  [(↑ (do E)) (↑ E)]
  [(↑ (E -> e)) (↑ E)]
  [(↑ (v -> E)) (↑ E)]
  [(↑ (E ▷ e)) (↑ E)]
  [(↑ (v ▷ E)) (↑ E)]
  [(↑ (♢ E)) (↑ E)]
  [(↑ (handle m E with v)) (↑ E)]
  [(↑ (handle m e with E)) (↑ E)]
  [(↑ (mon (k l j) E e)) (↑ E)]
  [(↑ (mon (k l j) v E)) (↑ E)]

  [(↑ (mark (k l j) (v_1 ▷ v_2) E))
      (mon (k l j) v_1 (↑ E))])

;; groups all marks pre-condition
(define-metafunction dependent-eval
  ↓ : any -> any
  [(↓ hole) hole]
  [(↓ (tuple E e)) (↓ E)]
  [(↓ (tuple v E)) (↓ E)]
  [(↓ (if E e_1 e_2)) (↓ E)]
  [(↓ (E e)) (↓ E)]
  [(↓ (v E)) (↓ E)]
  [(↓ (do E)) (↓ E)]
  [(↓ (E -> e)) (↓ E)]
  [(↓ (v -> E)) (↓ E)]
  [(↓ (E ▷ e)) (↓ E)]
  [(↓ (v ▷ E)) (↓ E)]
  [(↓ (♢ E)) (↓ E)]
  [(↓ (handle m E with v)) (↓ E)]
  [(↓ (handle m e with E)) (↓ E)]
  [(↓ (mon (k l j) E e)) (↓ E)]
  [(↓ (mon (k l j) v E)) (↓ E)]

  [(↓ (mark (k l j) (v_1 ▷ v_2) E))
      (in-hole (↓ E) (mon (k l j) v_2 hole))])

;; groups all marks pre-condition
(define-metafunction dependent-eval
  ↓↓ : v any -> any
  [(↓↓ v hole) hole]
  [(↓↓ v (tuple E e)) (↓↓ v E)]
  [(↓↓ v (tuple v E)) (↓↓ v E)]
  [(↓↓ v (if E e_1 e_2)) (↓↓ v E)]
  [(↓↓ v (E e)) (↓↓ v E)]
  [(↓↓ v (v E)) (↓↓ v E)]
  [(↓↓ v (do E)) (↓↓ v E)]
  [(↓↓ v (E -> e)) (↓↓ v E)]
  [(↓↓ v (v -> E)) (↓↓ v E)]
  [(↓↓ v (E ▷ e)) (↓↓ v E)]
  [(↓↓ v (v ▷ E)) (↓↓ v E)]
  [(↓↓ v (♢ E)) (↓↓ v E)]
  [(↓↓ v (handle m E with v)) (↓↓ v E)]
  [(↓↓ v (handle m e with E)) (↓↓ v E)]
  [(↓↓ v (mon (k l j) E e)) (↓↓ v E)]
  [(↓↓ v (mon (k l j) v E)) (↓↓ v E)]

  [(↓↓ v (mark (k l j) (v_1 ▷ v_2) E))
       (in-hole (↓↓ v E) (mon (l k j) v_2 hole))]

  [(↓↓ v (mark (k l j) (v_1 ▶ v_2) E))
       (in-hole (↓↓ v E) (mon (l k j) (v_2 e) hole))
       (where e (mon (k j j) v_1 (in-hole (↑ E) v)))])

(module+ test

  (redex-match? dependent-eval
               (in-hole E_2 (handle m E_3 with v_h))
               (term (handle ▷ hole with (λ (x) x))))

  (term (unhandled ▷ (handle ▷ hole with (λ (x) x))))

  (redex-match? dependent-eval
                (in-hole E_2 (handle ♢ E♢_3 with v_h))
                (term (handle ♢ hole with true)))

  (redex-match? dependent-eval
                (in-hole E_2 (handle ▷ E▷_3 with v_h))
                (term (handle ♢ (handle ▷ hole with false) with true)))

  (test-equal
    (term (↑ (mark (k l j) (λ (x) true)
                             (tuple (mark (l k j) (λ (y) false) hole) e))))
    (term (mon (k l j) (λ (x) true) (mon (l k j) (λ (y) false) hole))))
)
