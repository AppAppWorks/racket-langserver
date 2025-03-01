#lang racket/base

(provide open-docs
         uri->safe-doc)

; The shared resource for methods
(define open-docs (make-hasheq))

(define (uri->safe-doc uri)
  (hash-ref open-docs (string->symbol uri) #f))
