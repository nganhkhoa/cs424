#lang racket

(provide (all-defined-out))

(require redex/reduction-semantics)

(define-language core
  (e     ::= x b f (tuple e e) (if e e e) (e e))
  (b     ::= true false)
  (f     ::= o (λ (x) e))
  (o     ::= fst snd)
  (x y z ::= variable-not-otherwise-mentioned)
  #:binding-forms (λ (x) e #:refers-to x))

(define-extended-language contracts core
  (e     ::= .... κ (mon (k l j) e e))
  (κ     ::= b f (tuple e e) (e -> e))
  (j k l ::= variable-not-otherwise-mentioned))

(define-extended-language effects contracts
  (e     ::= .... (handle m e with e) (do e))
  (κ     ::= .... (e ▷ e) (♢ e))
  (m     ::= ▷ ♢))

(define-extended-language effects-eval effects
  (e     ::= .... (mark (k l j) v e) (err k j))
  (v     ::= b f (tuple v v) (v -> v) (v ▷ v) (♢ v))
  (E     ::= (tuple E e) (tuple v E) (if E e e) (E e) (v E)
             (handle m E with v) (do E) (E -> e) (v -> E)
             (E ▷ e) (v ▷ E) (♢ E) (mon (k l j) v E) (mark (k l j) v E)
             ;; different part
             hole (mon (k l j) E e) (handle m e with E))
  (E▷    ::= (tuple E e) (tuple v E) (if E e e) (E e) (v E)
             (handle m E with v) (do E) (E -> e) (v -> E)
             (E ▷ e) (v ▷ E) (♢ E) (mon (k l j) v E) (mark (k l j) v E)
             ;; different part
             hole (handle m e with E))
  (E♢    ::= (tuple E e) (tuple v E) (if E e e) (E e) (v E)
             (handle m E with v) (do E) (E -> e) (v -> E)
             (E ▷ e) (v ▷ E) (♢ E) (mon (k l j) v E) (mark (k l j) v E)
             ;; different part
             (mon (k l j) E e) (handle ♢ e with E) (handle ▷ e with E♢)))

(define-extended-language dependent-eval effects-eval
  (κ     ::= .... (e => e) (e ▶ e))
  (v     ::= .... (v => v) (v ▶ v))
  (E     ::= .... (E => e) (v => E) (E ▶ e) (v ▶ E))
  (E▷    ::= .... (E▷ => e) (v => E▷) (E▷ ▶ e) (v ▶ E▷))
  (E♢    ::= .... (E♢ => e) (v => E♢) (E♢ ▶ e) (v ▶ E♢)))

(default-language dependent-eval)

;; output pictures of language for debugging
;; (require redex/pict
;;          pict
;;          racket/draw)
;; (define (save-as-png lang filename)
;;   (let ([p (language->pict lang)])
;;     (send (pict->bitmap p) save-file filename 'png)))
;; (save-as-png core "core.png")
;; (save-as-png effects "efffects.png")
;; (save-as-png contracts "contracts.png")
;; (save-as-png effects-eval "effects-eval.png")
;; (save-as-png dependent-eval "dependent-eval.png")

(module+ test

  ;; basic stuffs
  (test-match dependent-eval f (term (λ (x) x)))

  ;; values
  (test-match dependent-eval v (term true))
  (test-match dependent-eval v (term true))
  (test-match dependent-eval v (term false))
  (test-match dependent-eval v (term (tuple true true)))
  (test-match dependent-eval v (term (λ (x) e)))
  (test-match dependent-eval v (term (true -> true)))
  (test-match dependent-eval v (term (true ▷ false)))
  (test-match dependent-eval v (term (♢ false)))

  ;; contracts

  ;; handle expressions
  (test-match dependent-eval (handle m v_1 with v_2) (term (handle ▷ (tuple true false) with (λ (x) x))))
  (test-match dependent-eval (handle m e_1 with e_2) (term (handle ▷ (do true) with (λ (x) (tuple x (λ (x) x))))))

  )
