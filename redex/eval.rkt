#lang racket

(provide (all-defined-out))

(require "base.rkt")
(require "meta.rkt")
(require "reduction.rkt")

(require redex/reduction-semantics)

(define-metafunction effects
  closed? : e y ... -> boolean
  [(closed? x y ... x z ...) #true]
  [(closed? x y ... ) #false]

  [(closed? b y ... ) #true]
  [(closed? o y ... ) #true]

  [(closed? (λ (x) e) y ...) (closed? e x y ...)]
  [(closed? (e_1 e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]

  [(closed? (tuple e_1 e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]

  [(closed? (if e_1 e_2 e_3) y ...)
   ,(and (term (closed? e_1 y ...))
         (term (closed? e_2 y ...))
         (term (closed? e_3 y ...)))]

  ;; fst snd
  [(closed? (o e) y ...) (closed? e y ...)]

  [(closed? (mon (k l j) e_1 e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]

  [(closed? (handle m e_1 with e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]

  [(closed? (do e) y ...) (closed? e y ...)]

  [(closed? (e_1 -> e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]

  [(closed? (e_1 ▷ e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]

  [(closed? (♢ e) y ...) (closed? e y ...)]

  [(closed? (e_1 => e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]

  [(closed? (e_1 ▶ e_2) y ...)
   ,(and (term (closed? e_1 y ...)) (term (closed? e_2 y ...)))]
  )

(define (eval t)
  (let ([result (apply-reduction-relation* ->* t)])
    (cond
      [(= (length result) 1)
       (define ans (first result))
       (cond
         [(redex-match? dependent-eval b ans) ans]
         ;; fall through
         [(redex-match? dependent-eval v ans) 'opaque]
         [(redex-match? dependent-eval (in-hole E (err k j)) ans)
          (redex-let dependent-eval
                     ([(in-hole E_1 (name err (err k j))) ans])
                     (term err))]
         [else 'error])]
      [else (raise "reduction bug")])))

;; Theorem 5.1
(define (is-partial e)
  (let ([ans (eval e)])
    (printf "check: ~s\n" e)
    (or (equal? ans (term true))
        (equal? ans (term false))
        (redex-match? dependent-eval (err k j) ans)
        (equal? ans 'opaque)
        (equal? ans 'error))))

(define (test-theorem-5-1 num-valid-tests max-depth)
  (let loop ([valid-count 0]
             [total-generated 0])
    (when (< valid-count num-valid-tests)
      (define random-e (generate-term effects e max-depth))
      (if (equal? (term (closed? ,random-e)) #true)
          (begin
            ;; (printf "[~a] Testing closed program: ~v\n" (add1 valid-count) random-e)
            (unless (is-partial random-e)
              (error 'theorem-failed "Counterexample found: ~v" random-e))

            (loop (add1 valid-count) (add1 total-generated)))

          (loop valid-count (add1 total-generated))))))

(module+ test
  (test-equal (eval (term false)) (term false))
  (test-equal (eval (term true)) (term true))
  (test-equal (eval (term (λ (x) x))) 'opaque)

  (test-equal (eval (term (mon (k l j) false (tuple true false)))) (term (err k j)))

  ;; (redex-check effects e
  ;;              (is-partial (term e))
  ;;              #:prepare (λ (e)
  ;;                          (printf "prepare: ~s ~s\n" e (term (closed? e)))
  ;;                          (if (term (closed? e)) e (term true)))
  ;;              #:attempts 100000)

  (test-theorem-5-1 1000 10)
)
