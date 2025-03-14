#lang racket/base

(require "interface.rkt"
         "../autocomplete.rkt"
         racket/class
         racket/hash)

(provide completion%)

(define completion%
  (class base-service%
    (super-new)
    (define completions (hash))

    (define/override (get)
      completions)

    (define/override (reset)
      (set! completions (hash)))

    (define/override (walk-stx stx expanded-stx)
      (define c (hash-union (walk expanded-stx)
                            (walk-module expanded-stx) #:combine (λ (a _b) a)))
                            (set! completions c))))

