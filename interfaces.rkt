#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/contract/base
         racket/match
         "json-util.rkt")

(provide WorkspaceEdit
         TextEdit
         CodeAction
         Diagnostic
         Command
         Pos
         Range
         abs-pos->Pos)

(define-json-expander WorkspaceEdit
  (#:required)
  (#:optional
   [changes any/c]
   [documentChanges list?]
   [changeAnnotations any/c]))
(define-json-expander TextEdit
  (#:required
   [range any/c]
   [newText string?])
  (#:optional))
(define-json-expander CodeAction
  (#:required
   [title string?])
  (#:optional
   [kind string?]
   [diagnostics list?]
   [isPreferred boolean?]
   [edit any/c]
   [data any/c]))
(define-json-expander Diagnostic
  (#:required
   [range any/c])
  (#:optional
   [severity (or/c 1 2 3 4)]
   [source string?]
   [message string?]))
(define-json-expander Command
  (#:required
   [title string?]
   [command string?])
  (#:optional
   [arguments list?]))

(define-match-expander Pos
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hash* ['line (? exact-nonnegative-integer? l)]
                ['character (? exact-nonnegative-integer? c)]))]))
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hasheq 'line l
                 'character c))])))

(define-json-expander Range
  (#:required
   [start any/c]
   [end any/c])
  (#:optional))

(define (abs-pos->Pos editor pos)
  (define-values (line char) (send editor pos->line/char pos))
  (Pos #:line line #:char char))

