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
    [--> (mon (k l j) (v_1 -> v_2) f) (λ (x) (mon (k l j) v_2 (f (mon (l k j) v_1 x))))
         GRD-FUN]))


(define ->effects
  (extend-reduction-relation ->contract dependent-eval
    [--> (handle m v with v_h) v
         HANDLE]

    ;; the other rules need side-condition with some metafunction
    ;; is-unhandled collect-pre collect-post

    ;; ATTENTION:
    ;; DO▷ in Fig. 3 and Section 4 (dependent) are different

    ;; Fig. 3 no dependent contract
    ;; [--> (handle ▷ (in-hole E▷ (do v)) with v_h)
    ;;      ((v_h e_v) (λ (x) (handle ▷ (in-hole E▷ e_x) with v_h)))

    ;;      (side-condition (term (unhandled ▷ E▷)))
    ;;      (where e_v (in-hole (↑ E▷) v))
    ;;      (where e_x (in-hole (↓ E▷) x))
    ;;      DO▷]

    [--> (handle ▷ (in-hole E▷ (do v)) with v_h)
         ((v_h e_v) (λ (x) (handle ▷ (in-hole E▷ e_x) with v_h)))

         (side-condition (term (unhandled ▷ E▷)))
         (where e_v (in-hole (↑ E▷) v))
         (where e_x (in-hole (↓↓ v E▷) x))
         DO▷]

    [--> (handle ♢ (in-hole E♢ (do v)) with (tuple v_1 v_2))
         (handle ♢ (in-hole E♢ v_1) with v_2)

         (side-condition (term (unhandled ♢ E♢)))
         DO-PAIR♢]

    [--> (handle ♢ (in-hole E♢ (do v)) with f)
         (handle ♢ (in-hole E♢ (do v)) with (f v))

         (side-condition (term (unhandled ♢ E♢)))
         DO-FUN♢]
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
  ;; --------------------------
  ;; core
  ;; --------------------------

  ;; IF-TRUE
  (test--> ->core
           (term (if true false true))
           (term false))
  (test--> ->core
           (term (if (tuple true false) true false))
           (term true))
  (test--> ->core
           (term (if (λ (x) x) (tuple true false) false))
           (term (tuple true false)))

  ;; IF-FALSE
  (test--> ->core
           (term (if false true false))
           (term false))
  (test--> ->core
           (term (if false (tuple true false) true))
           (term true))
  (test--> ->core
           (term (if false ((λ (x) x) true) (tuple false true)))
           (term (tuple false true)))

  ;; APP-LAMBDA
  (test--> ->core
           (term ((λ (x) x) true))
           (term true))
  (test--> ->core
           (term ((λ (x) (tuple x false)) true))
           (term (tuple true false)))
  (test--> ->core
           (term ((λ (x) ((λ (y) x) false)) true))
           (term ((λ (y) true) false)))

  ;; APP-OP
  (test--> ->core
           (term (fst (tuple true false)))
           (term true))
  (test--> ->core
           (term (snd (tuple true false)))
           (term false))
  (test--> ->core
           (term (fst (tuple (λ (x) x) true)))
           (term (λ (x) x)))

  ;; --------------------------
  ;; contract
  ;; --------------------------

  ;; MON-TRUE
  (test--> ->contract
           (term (mon (k l j) true true))
           (term true))
  (test--> ->contract
           (term (mon (k l j) true false))
           (term false))
  (test--> ->contract
           (term (mon (k l j) true (tuple true false)))
           (term (tuple true false)))

  ;; MON-FALSE
  (test--> ->contract
           (term (mon (k l j) false true))
           (term (err k j)))
  (test--> ->contract
           (term (mon (k l j) false false))
           (term (err k j)))
  (test--> ->contract
           (term (mon (k l j) false (tuple true false)))
           (term (err k j)))

  ;; MON-FLAT
  (test--> ->contract
         (term (mon (k l j) (λ (x) true) false))
         (term (mon (k l j) ((λ (x) true) false) false)))
  (test--> ->contract
         (term (mon (k l j) (λ (x) false) true))
         (term (mon (k l j) ((λ (x) false) true) true)))
  (test--> ->contract
         (term (mon (k l j) fst (tuple true false)))
         (term (mon (k l j) (fst (tuple true false)) (tuple true false))))

  ;; MON-PAIR
  (test--> ->contract
           (term (mon (k l j) (tuple true true) true))
           (term (err k j)))
  (test--> ->contract
           (term (mon (k l j) (tuple true false) false))
           (term (err k j)))
  (test--> ->contract
           (term (mon (k l j) (tuple true true) (λ (x) x)))
           (term (err k j)))

  ;; GRD-PAIR
  (test--> ->contract
           (term (mon (k l j) (tuple true true) (tuple false true)))
           (term (tuple (mon (k l j) true false)
                        (mon (k l j) true true))))
  (test--> ->contract
           (term (mon (k l j) (tuple false true) (tuple true false)))
           (term (tuple (mon (k l j) false true)
                        (mon (k l j) true false))))
  (test--> ->contract
           (term (mon (k l j) (tuple (λ (x) true) true) (tuple false true)))
           (term (tuple (mon (k l j) (λ (x) true) false)
                        (mon (k l j) true true))))

  ;; MON-FUN
  (test--> ->contract
           (term (mon (k l j) (true -> true) true))
           (term (err k j)))
  (test--> ->contract
           (term (mon (k l j) (true -> false) false))
           (term (err k j)))
  (test--> ->contract
           (term (mon (k l j) (true -> true) (tuple true false)))
           (term (err k j)))

  ;; GRD-FUN
  (test--> ->contract
           (term (mon (k l j) (true -> true) fst))
           (term (λ (x) (mon (k l j) true (fst (mon (l k j) true x))))))
  (test--> ->contract
           (term (mon (k l j) (false -> true) snd))
           (term (λ (x) (mon (k l j) true (snd (mon (l k j) false x))))))
  (test--> ->contract
           (term (mon (k l j) (true -> false) (λ (y) (tuple y true))))
           (term (λ (x) (mon (k l j) false
                             ((λ (y) (tuple y true))
                              (mon (l k j) true x))))))

  ;; --------------------------
  ;; effects
  ;; --------------------------

  ;; HANDLE
  (test--> ->effects
           (term (handle ▷ true with false))
           (term true))
  (test--> ->effects
           (term (handle ♢ (tuple true false) with true))
           (term (tuple true false)))
  (test--> ->effects
           (term (handle ▷ (λ (x) x) with (tuple true false)))
           (term (λ (x) x)))

  ;; DO▷
  (test--> ->effects
           (term (handle ▷ (do true)
                         with (λ (r) (λ (k) (k r)))))
           (term (((λ (r) (λ (k) (k r))) true)
                  (λ (x) (handle ▷ x
                                 with (λ (r) (λ (k) (k r))))))))
  (test--> ->effects
           (term (handle ▷ (tuple (do true) false)
                         with (λ (r) (λ (k) (k r)))))
           (term (((λ (r) (λ (k) (k r))) true)
                  (λ (x) (handle ▷ (tuple x false)
                                 with (λ (r) (λ (k) (k r))))))))
  (test--> ->effects
           (term (handle ▷ (if (do true) false true)
                         with (λ (r) (λ (k) (k r)))))
           (term (((λ (r) (λ (k) (k r))) true)
                  (λ (x) (handle ▷ (if x false true)
                                 with (λ (r) (λ (k) (k r))))))))

  ;; DO-PAIR♢
  (test--> ->effects
           (term (handle ♢ (mon (k l j) (do true) true)
                         with (tuple false false)))
           (term (handle ♢ (mon (k l j) false true)
                         with false)))
  (test--> ->effects
           (term (handle ♢ (mon (k l j) (do true) true)
                         with (tuple true false)))
           (term (handle ♢ (mon (k l j) true true)
                         with false)))
  (test--> ->effects
           (term (handle ♢ (mon (k l j) (do false) true)
                         with (tuple (tuple true false) true)))
           (term (handle ♢ (mon (k l j) (tuple true false) true)
                         with true)))

  ;; DO-FUN♢
  (test--> ->effects
           (term (handle ♢ (mon (k l j) (do true) true)
                         with (λ (x) (tuple x false))))
           (term (handle ♢ (mon (k l j) (do true) true)
                         with ((λ (x) (tuple x false)) true))))
  (test--> ->effects
           (term (handle ♢ (mon (k l j) (do false) true)
                         with (λ (x) (tuple true x))))
           (term (handle ♢ (mon (k l j) (do false) true)
                         with ((λ (x) (tuple true x)) false))))
  (test--> ->effects
           (term (handle ♢ (mon (k l j) (do true) true)
                         with (λ (x) (tuple false false))))
           (term (handle ♢ (mon (k l j) (do true) true)
                         with ((λ (x) (tuple false false)) true))))

  ;; --------------------------
  ;; effects-eval
  ;; --------------------------

  ;; MON-HANDLE▷
  (test--> ->effects-eval
           (term (mon (k l j) (true ▷ true) true))
           (term (err k j)))
  (test--> ->effects-eval
           (term (mon (k l j) (false ▷ true) false))
           (term (err k j)))
  (test--> ->effects-eval
           (term (mon (k l j) (true ▷ false) (tuple true false)))
           (term (err k j)))

  ;; GRD-HANDLE▷
  (test--> ->effects-eval
           (term (mon (k l j) (true ▷ false) fst))
           (term (λ (x) (mark (k l j) (true ▷ false) (fst x)))))
  (test--> ->effects-eval
           (term (mon (k l j) (false ▷ true) snd))
           (term (λ (x) (mark (k l j) (false ▷ true) (snd x)))))
  (test--> ->effects-eval
           (term (mon (k l j) (true ▷ true) (λ (y) (do y))))
           (term (λ (x) (mark (k l j) (true ▷ true)
                              ((λ (y) (do y)) x)))))

  ;; MARK
  (test--> ->effects-eval
           (term (mark (k l j) true false))
           (term false))
  (test--> ->effects-eval
           (term (mark (k l j) (true ▷ false) (tuple true false)))
           (term (tuple true false)))
  (test--> ->effects-eval
           (term (mark (k l j) (♢ true) (λ (x) x)))
           (term (λ (x) x)))

  ;; MON-HANDLE♢
  (test--> ->effects-eval
           (term (mon (k l j) (♢ true) true))
           (term (err k j)))
  (test--> ->effects-eval
           (term (mon (k l j) (♢ false) false))
           (term (err k j)))
  (test--> ->effects-eval
           (term (mon (k l j) (♢ (tuple true false)) (tuple true false)))
           (term (err k j)))

  ;; GRD-HANDLE♢
  (test--> ->effects-eval
           (term (mon (k l j) (♢ (tuple true false)) fst))
           (term (λ (x) (handle ♢ (fst x) with (tuple true false)))))
  (test--> ->effects-eval
           (term (mon (k l j) (♢ (tuple false true)) snd))
           (term (λ (x) (handle ♢ (snd x) with (tuple false true)))))
  (test--> ->effects-eval
           (term (mon (k l j) (♢ (λ (r) (tuple r false))) (λ (y) y)))
           (term (λ (x) (handle ♢ ((λ (y) y) x)
                                 with (λ (r) (tuple r false))))))

  ;; --------------------------
  ;; dependent-eval
  ;; --------------------------

  ;; MON-DEP-FUN
  (test--> ->dependent-eval
           (term (mon (k l j) (true => (λ (y) true)) true))
           (term (err k j)))
  (test--> ->dependent-eval
           (term (mon (k l j) (false => (λ (y) true)) false))
           (term (err k j)))
  (test--> ->dependent-eval
           (term (mon (k l j) (true => (λ (y) false)) (tuple true false)))
           (term (err k j)))

  ;; GRD-DEP-FUN
  (test--> ->dependent-eval
           (term (mon (k l j) (true => (λ (y) true)) fst))
           (term (λ (x)
                   (mon (k l j)
                        ((λ (y) true) (mon (l j j) true x))
                        (fst (mon (l k j) true x))))))
  (test--> ->dependent-eval
           (term (mon (k l j) (false => (λ (y) true)) snd))
           (term (λ (x)
                   (mon (k l j)
                        ((λ (y) true) (mon (l j j) false x))
                        (snd (mon (l k j) false x))))))
  (test--> ->dependent-eval
           (term (mon (k l j) (true => (λ (y) y)) (λ (z) z)))
           (term (λ (x)
                   (mon (k l j)
                        ((λ (y) y) (mon (l j j) true x))
                        ((λ (z) z) (mon (l k j) true x))))))

  ;; MON-HANDLE▶
  (test--> ->dependent-eval
           (term (mon (k l j) (true ▶ (λ (r) true)) true))
           (term (err k j)))
  (test--> ->dependent-eval
           (term (mon (k l j) (false ▶ (λ (r) true)) false))
           (term (err k j)))
  (test--> ->dependent-eval
           (term (mon (k l j) (true ▶ (λ (r) false)) (tuple true false)))
           (term (err k j)))

  ;; GRD-HANDLE▶
  (test--> ->dependent-eval
           (term (mon (k l j) (true ▶ (λ (r) true)) fst))
           (term (λ (x) (mark (k l j) (true ▶ (λ (r) true)) (fst x)))))
  (test--> ->dependent-eval
           (term (mon (k l j) (false ▶ (λ (r) true)) snd))
           (term (λ (x) (mark (k l j) (false ▶ (λ (r) true)) (snd x)))))
  (test--> ->dependent-eval
           (term (mon (k l j) (true ▶ (λ (r) false)) (λ (y) (do y))))
           (term (λ (x) (mark (k l j) (true ▶ (λ (r) false))
                              ((λ (y) (do y)) x))))))
