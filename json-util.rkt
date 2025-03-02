#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/dict
         racket/match
         syntax/parse
         json)

(define-syntax (define-json-expander stx)
  (syntax-parse stx
    [(_ name:id (#:required [key:id ctc:expr] ...) (#:optional [key-opt:id ctc-opt:expr] ...))
     (with-syntax ([(key_ ...) (generate-temporaries #'(key ...))]
                   [(keyword ...)
                    (for/list ([k (syntax->datum #'(key ...))])
                      (string->keyword (symbol->string k)))]
                   [(key-opt_ ...) (generate-temporaries #'(key-opt ...))]
                   [(key-opt_default ...) (generate-temporaries #'(key-opt ...))]
                   [(keyword-opt ...)
                    (for/list ([k (syntax->datum #'(key-opt ...))])
                      (string->keyword (symbol->string k)))]
                   [~?-id (quote-syntax ~?)])
       (syntax/loc stx
         (define-match-expander name
           (λ (stx)
             (syntax-parse stx
               [(_ (~optional (~seq keyword key_)) ...
                   (~optional (~seq keyword-opt key-opt_ (~optional (~seq (~datum ??) key-opt_default)))) ...)
                (quasisyntax/loc stx
                  (hash* (~?-id ['key (? ctc key_)]) ...
                         (~?-id ['key-opt (? ctc-opt key-opt_) #:default key-opt_default]
                                (~?-id ['key-opt (? ctc-opt key-opt_)])) ...))]))
           (λ (stx)
             (syntax-parse stx
               [(_ (~seq keyword key_) ... (~optional (~seq keyword-opt key-opt_)) ...)
                (syntax/loc stx
                  (make-hasheq (list (cons 'key key_) ... (~?-id (cons 'key-opt key-opt_)) ...)))])))))]))

(define (jsexpr-has-key? jsexpr keys)
  (cond [(null? keys) #t]
        [else (and (hash-has-key? jsexpr (car keys))
                   (jsexpr-has-key? (hash-ref jsexpr (car keys)) (cdr keys)))]))

(define (jsexpr-ref jsexpr keys)
  (cond [(null? keys) jsexpr]
        [else (jsexpr-ref (hash-ref jsexpr (car keys)) (cdr keys))]))

(define (jsexpr-set jsexpr keys v)
  (cond [(null? keys) jsexpr]
        [(null? (cdr keys)) (hash-set jsexpr (car keys) v)]
        [else (hash-set jsexpr (car keys)
                        (jsexpr-set (hash-ref jsexpr (car keys)) (cdr keys) v))]))

(define (jsexpr-remove jsexpr keys)
  (cond [(null? keys) jsexpr]
        [(null? (cdr keys)) (hash-remove jsexpr (car keys))]
        [else (hash-set jsexpr (car keys)
                        (jsexpr-remove (hash-ref jsexpr (car keys)) (cdr keys)))]))

(define (jsobj? x)
  (and (dict? x) (jsexpr? x)))

(provide define-json-expander
         jsexpr-has-key?
         jsexpr-ref
         jsexpr-set
         jsexpr-remove
         jsobj?)
