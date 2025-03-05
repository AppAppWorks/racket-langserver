#lang racket/base
(require
  json
  racket/match
  racket/contract
  racket/class
  "error-codes.rkt"
  "interfaces.rkt"
  "responses.rkt"
  "doc.rkt")
(require "open-docs.rkt")

(define (resolve id params)
  (match params
    [(CodeAction #:title title #:kind "source.organizeImports" #:data uri)
     (remove-unused-requires id title uri)]
    [_ (error-response id INVALID-PARAMS "codeAction/resolve failed")]))

(define (remove-unused-requires id title uri)
  (define safe-doc (uri->safe-doc uri))
  (with-read-doc safe-doc
    (λ (doc)
      (define unused-reqs (send (Doc-trace doc) get-unused-reqs))
      (success-response id
                        (CodeAction #:title title
                                    #:kind "source.organizeImports"
                                    #:edit (WorkspaceEdit
                                            #:changes
                                            (hasheq (string->symbol uri)
                                                    (unused-reqs->text-edits (Doc-text doc)
                                                                             unused-reqs))))))))

; modified from DrRacket gui.rkt's remove-unused-requires
(define (unused-reqs->text-edits txt unused-reqs)
  (hash-map
   unused-reqs
   (λ (start end)
     (define prev-token-end (find-preceding-ws-pos txt start))
     (TextEdit #:range (Range #:start (abs-pos->Pos txt prev-token-end)
                              #:end (abs-pos->Pos txt end))
               #:newText ""))))

; direct copy from DrRacket gui.rkt
(define (find-preceding-ws-pos edit pos)
  (let loop ([token-type (send edit classify-position (sub1 pos))]
             [current-pos pos])
    (cond
      [(eq? token-type 'white-space)
       (define-values (ws-start _ws-end)
         (send edit get-token-range (sub1 current-pos)))
       (loop (send edit classify-position (sub1 ws-start)) ws-start)]
      [(and (eq? token-type 'comment)
            (char=? (send edit get-char current-pos) #\newline))
       (add1 current-pos)]
      [else current-pos])))

(provide
 (contract-out
  [resolve (exact-nonnegative-integer? jsexpr? . -> . jsexpr?)]))