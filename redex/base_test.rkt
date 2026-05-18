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
  ;; --------------------------
  ;; core
  ;; --------------------------

  ;; e ::= x
  (test-match dependent-eval e (term x))
  (test-match dependent-eval e (term y))
  (test-match dependent-eval e (term z))

  ;; e ::= b
  (test-match dependent-eval e (term true))
  (test-match dependent-eval e (term false))
  (test-match dependent-eval e (term (if true false true)))

  ;; e ::= f
  (test-match dependent-eval e (term fst))
  (test-match dependent-eval e (term snd))
  (test-match dependent-eval e (term (λ (x) x)))

  ;; e ::= (tuple e e)
  (test-match dependent-eval e (term (tuple true false)))
  (test-match dependent-eval e (term (tuple x y)))
  (test-match dependent-eval e (term (tuple (if true false true) (λ (x) x))))

  ;; e ::= (if e e e)
  (test-match dependent-eval e (term (if true false true)))
  (test-match dependent-eval e (term (if x y z)))
  (test-match dependent-eval e (term (if (tuple true false) (λ (x) x) snd)))

  ;; e ::= (e e)
  (test-match dependent-eval e (term ((λ (x) x) true)))
  (test-match dependent-eval e (term (fst (tuple true false))))
  (test-match dependent-eval e (term ((if true (λ (x) x) (λ (x) y)) false)))

  ;; b ::= true | false
  (test-match dependent-eval b (term true))
  (test-match dependent-eval b (term false))
  (test-no-match dependent-eval b (term x))

  ;; f ::= o | (λ (x) e)
  (test-match dependent-eval f (term fst))
  (test-match dependent-eval f (term snd))
  (test-match dependent-eval f (term (λ (x) (if x true false))))

  ;; o ::= fst | snd
  (test-match dependent-eval o (term fst))
  (test-match dependent-eval o (term snd))
  (test-no-match dependent-eval o (term (λ (x) x)))

  ;; --------------------------
  ;; contracts
  ;; --------------------------

  ;; κ ::= b
  (test-match dependent-eval κ (term true))
  (test-match dependent-eval κ (term false))
  (test-no-match dependent-eval κ (term x))

  ;; κ ::= f
  (test-match dependent-eval κ (term fst))
  (test-match dependent-eval κ (term snd))
  (test-match dependent-eval κ (term (λ (x) x)))

  ;; κ ::= (tuple e e)
  (test-match dependent-eval κ (term (tuple true false)))
  (test-match dependent-eval κ (term (tuple x y)))
  (test-match dependent-eval κ (term (tuple (λ (x) x) true)))

  ;; κ ::= (e -> e)
  (test-match dependent-eval κ (term (true -> false)))
  (test-match dependent-eval κ (term ((λ (x) x) -> true)))
  (test-match dependent-eval κ (term ((tuple true false) -> (tuple false true))))

  ;; e ::= κ
  (test-match dependent-eval e (term (true -> false)))
  (test-match dependent-eval e (term (tuple true false)))
  (test-match dependent-eval e (term (λ (x) x)))

  ;; e ::= (mon (k l j) e e)
  (test-match dependent-eval e (term (mon (k l j) true false)))
  (test-match dependent-eval e (term (mon (server contract client) (true -> false) (λ (x) x))))
  (test-match dependent-eval e (term (mon (k l j) (tuple true false) (tuple true false))))

  ;; --------------------------
  ;; effects
  ;; --------------------------

  ;; e ::= (handle m e with e)
  (test-match dependent-eval e (term (handle ▷ true with (λ (x) x))))
  (test-match dependent-eval e (term (handle ♢ (do true) with (λ (x) (tuple x x)))))
  (test-match dependent-eval e (term (handle ▷ (tuple true false) with fst)))

  ;; e ::= (do e)
  (test-match dependent-eval e (term (do true)))
  (test-match dependent-eval e (term (do x)))
  (test-match dependent-eval e (term (do (tuple true false))))

  ;; κ ::= (e ▷ e)
  (test-match dependent-eval κ (term (true ▷ false)))
  (test-match dependent-eval κ (term ((λ (x) x) ▷ true)))
  (test-match dependent-eval κ (term ((tuple true false) ▷ (tuple false true))))

  ;; κ ::= (♢ e)
  (test-match dependent-eval κ (term (♢ true)))
  (test-match dependent-eval κ (term (♢ (λ (x) x))))
  (test-match dependent-eval κ (term (♢ (tuple true false))))

  ;; m ::= ▷ | ♢
  (test-match dependent-eval m (term ▷))
  (test-match dependent-eval m (term ♢))
  (test-no-match dependent-eval m (term true))

  ;; --------------------------
  ;; effects-eval
  ;; --------------------------

  ;; e ::= (mark (k l j) v e)
  (test-match dependent-eval e (term (mark (k l j) true false)))
  (test-match dependent-eval e (term (mark (srv ctr cli) (true ▷ false) (do true))))
  (test-match dependent-eval e (term (mark (k l j) (♢ true) (tuple true false))))

  ;; e ::= (err k j)
  (test-match dependent-eval e (term (err k j)))
  (test-match dependent-eval e (term (err server client)))
  (test-match dependent-eval e (term (err blame source)))

  ;; v ::= b
  (test-match dependent-eval v (term true))
  (test-match dependent-eval v (term false))
  (test-no-match dependent-eval v (term x))

  ;; v ::= f
  (test-match dependent-eval v (term fst))
  (test-match dependent-eval v (term snd))
  (test-match dependent-eval v (term (λ (x) x)))

  ;; v ::= (tuple v v)
  (test-match dependent-eval v (term (tuple true false)))
  (test-match dependent-eval v (term (tuple fst snd)))
  (test-match dependent-eval v (term (tuple (λ (x) x) (tuple true false))))

  ;; v ::= (v -> v)
  (test-match dependent-eval v (term (true -> false)))
  (test-match dependent-eval v (term (fst -> snd)))
  (test-match dependent-eval v (term ((tuple true false) -> (tuple false true))))

  ;; v ::= (v ▷ v)
  (test-match dependent-eval v (term (true ▷ false)))
  (test-match dependent-eval v (term (fst ▷ snd)))
  (test-match dependent-eval v (term ((tuple true false) ▷ (tuple false true))))

  ;; v ::= (♢ v)
  (test-match dependent-eval v (term (♢ true)))
  (test-match dependent-eval v (term (♢ fst)))
  (test-match dependent-eval v (term (♢ (tuple true false))))

  ;; E
  (test-match dependent-eval E (term hole))
  (test-match dependent-eval E (term (tuple hole true)))
  (test-match dependent-eval E (term (handle ▷ hole with (λ (x) x))))

  ;; E▷
  (test-match dependent-eval E▷ (term hole))
  (test-match dependent-eval E▷ (term (tuple hole true)))
  (test-match dependent-eval E▷ (term (handle ♢ hole with fst)))

  ;; E♢
  (test-match dependent-eval E♢(term (handle ♢ true with hole)))
  (test-match dependent-eval E♢(term (handle ▷ true with (handle ♢ false with hole))))
  (test-match dependent-eval E♢(term (handle ▷ true with (mon (k l j) hole false))))

  ;; --------------------------
  ;; dependent-eval
  ;; --------------------------

  ;; κ ::= (e => e)
  (test-match dependent-eval κ (term (true => false)))
  (test-match dependent-eval κ (term ((λ (x) x) => (λ (y) y))))
  (test-match dependent-eval κ (term ((tuple true false) => true)))

  ;; κ ::= (e ▶ e)
  (test-match dependent-eval κ (term (true ▶ false)))
  (test-match dependent-eval κ (term ((λ (x) x) ▶ (λ (y) y))))
  (test-match dependent-eval κ (term ((tuple true false) ▶ true)))

  ;; v ::= (v => v)
  (test-match dependent-eval v (term (true => false)))
  (test-match dependent-eval v (term (fst => snd)))
  (test-match dependent-eval v (term ((tuple true false) => (tuple false true))))

  ;; v ::= (v ▶ v)
  (test-match dependent-eval E♢(term ((mon (k l j) hole true) ▶ false)))
  (test-match dependent-eval E♢(term ((handle ♢ true with hole) ▶ false)))
  (test-match dependent-eval E♢(term (true ▶ (mon (k l j) hole false))))

  ;; E ::= (E => e) | (v => E) | (E ▶ e) | (v ▶ E)
  (test-match dependent-eval E (term (hole => true)))
  (test-match dependent-eval E (term (true => hole)))
  (test-match dependent-eval E (term (hole ▶ false)))

  ;; E▷ ::= dependent variants
  (test-match dependent-eval E▷ (term (hole => true)))
  (test-match dependent-eval E▷ (term (true => hole)))
  (test-match dependent-eval E▷ (term (hole ▶ false)))

  ;; E♢ ::= dependent variants
  (test-match dependent-eval E♢(term ((mon (k l j) hole true) ▶ false)))
  (test-match dependent-eval E♢(term ((handle ♢ true with hole) ▶ false)))
  (test-match dependent-eval E♢(term (true ▶ (mon (k l j) hole false))))
)