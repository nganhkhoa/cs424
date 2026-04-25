#lang racket

(require redex)

(define-language core
  (e     ::= x b f (tuple e e) (if e e e) (e e) κ (mon (k l j) e e))
  (b     ::= true false)
  (f     ::= o (λ (x) e))
  (o     ::= fst snd)
  (x y z ::= variable-not-otherwise-mentioned)
  #:binding-forms (λ (e) e #:refers-to e))

(define-language contracts-part
  (e     ::= κ (mon (k l j) e e))
  (κ     ::= b f (tuple e e) (e -> e))
  (j k l ::= string))
(define-union-language contracts core contracts-part)

(define-language effects-part
  (e     ::= (handle m e with e) (do e))
  (κ     ::= (e ▷ e) (♢ e))
  (m     ::= ▷ ♢))
(define-union-language effects contracts effects-part)

(define-language effects-eval-part
  (e     ::= (mark (k l j) v e) (err k j))
  (v     ::= b f (tuple e e) (v -> v) (v ▷ v) (♢ v))
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

(define-union-language effects-eval effects effects-eval-part)
