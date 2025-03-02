#lang racket
(provide didRenameFiles didChangeWorkspaceFolders)
(require "json-util.rkt"
         "doc.rkt"
         "scheduler.rkt")
(require "open-docs.rkt")

(define-json-expander FileRename
  (#:required
   [oldUri string?]
   [newUri string?])
  (#:optional))
(define-json-expander  RenameFilesParams
  (#:required
   [files (listof jsobj?)])
  (#:optional))

(define-json-expander WorkspaceFolder
  (#:required
   [uri string?]
   [name string?])
  (#:optional))
(define-json-expander WorkspaceFoldersChangeEvent
  (#:required
   [added (listof jsobj?)]
   [removed (listof jsobj?)])
  (#:optional))

(define workspace-folders (mutable-set))

(define (didRenameFiles params)
  (match-define (RenameFilesParams #:files files) params)
  (for ([f files])
    (match-define (FileRename #:oldUri old-uri #:newUri new-uri) f)

    ; remove all awaiting internal queries about `old-uri`
    (clear-old-queries/doc-close old-uri)

    (if (string-suffix? new-uri ".rkt")
      (let ([safe-doc (uri->safe-doc old-uri)])
        ; `safe-doc = #f` should be rarely happened.
        ; we simply give up to handle it, let's trust LSP client will send others request about analysis this file.
        (when safe-doc
          (with-write-doc safe-doc
            (lambda (doc)
              (doc-update-uri! doc new-uri)))
          (hash-set! open-docs (string->symbol new-uri) safe-doc)))
      (hash-remove! open-docs (string->symbol old-uri)))))

(define (didChangeWorkspaceFolders params)
  (match-define (hash* ['event (WorkspaceFoldersChangeEvent #:added added #:removed removed)]) params)
  (for ([f added])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (set-add! workspace-folders uri))
  (for ([f removed])
    (match-define (WorkspaceFolder #:uri uri #:name _) f)
    (set-remove! workspace-folders uri)))
