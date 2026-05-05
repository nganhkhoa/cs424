#lang racket

(provide (all-defined-out))

(require "base.rkt")

(require redex/reduction-semantics)

;; this file provides metafunctions for reduction rules

(define-metafunction effects-eval
  delta : o v -> v
  [(delta fst (tuple v_1 v_2)) v_1]
  [(delta snd (tuple v_1 v_2)) v_2])
