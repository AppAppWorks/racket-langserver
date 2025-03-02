#lang racket/base
(require json
         racket/dict
         racket/contract/base
         "json-util.rkt")

(provide
 InitializeParams
 ClientCapabilities
 TextDocumentClientCapabilities
 RenameClientCapabilities
 ServerCapabilities
 RenameOptions
 SemanticTokensOptions
 SemanticTokensLegend
 CodeActionOptions)

(define-json-expander InitializeParams
  (#:required
   [processId (or/c integer? (json-null))]
   [rootUri (or/c string? (json-null))]
   [capabilities jsobj?])
  (#:optional
   [clientInfo jsobj?]
   [locale string?]
   [rootPath (or/c string? (json-null))]
   [initializationOptions jsexpr?]
   [trace (one-of/c "off" "messages" "verbose")]
   [workspaceFolders (or/c (listof jsobj?) (json-null))]))

(define-json-expander ClientCapabilities
  (#:required)
  (#:optional
   [workspace jsobj?]
   [textDocument jsobj?]
   [notebookDocument jsobj?]
   [window jsobj?]
   [general jsobj?]
   [experimental jsexpr?]))

(define-json-expander TextDocumentClientCapabilities
  (#:required)
  (#:optional
   [synchronization jsobj?]
   [completion jsobj?]
   [hover jsobj?]
   [signatureHelp jsobj?]
   [declaration jsobj?]
   [definition jsobj?]
   [typeDefinition jsobj?]
   [implementation jsobj?]
   [references jsobj?]
   [documentHighlight jsobj?]
   [documentSymbol jsobj?]
   [codeAction jsobj?]
   [codeLens jsobj?]
   [documentLink jsobj?]
   [colorProvider jsobj?]
   [formatting jsobj?]
   [rangeFormatting jsobj?]
   [onTypeFormatting jsobj?]
   [rename jsobj?]
   [publishDiagnostics jsobj?]
   [foldingRange jsobj?]
   [selectionRange jsobj?]
   [linkedEditingRange jsobj?]
   [callHierarchy jsobj?]
   [semanticTokens jsobj?]
   [moniker jsobj?]
   [typeHierarchy jsobj?]
   [inlineValue jsobj?]
   [inlayHint jsobj?]
   [diagnostic jsobj?]))

(define-json-expander RenameClientCapabilities
  (#:required)
  (#:optional
   [dynamicRegistration boolean?]
   [prepareSupport boolean?]
   [prepareSupportDefaultBehavior 1]
   [honorsChangeAnnotations boolean?]))

(define-json-expander ServerCapabilities
  (#:required)
  (#:optional
   [positionEncoding string?]
   [textDocumentSync (or/c jsobj? (one-of/c 0 1 2))]
   [notebookDocumentSync jsobj?]
   [completionProvider jsobj?]
   [hoverProvider (or/c boolean? jsobj?)]
   [signatureHelpProvider jsobj?]
   [declarationProvider (or/c boolean? jsobj?)]
   [definitionProvider (or/c boolean? jsobj?)]
   [typeDefinitionProvider (or/c boolean? jsobj?)]
   [implementationProvider (or/c boolean? jsobj?)]
   [referencesProvider (or/c boolean? jsobj?)]
   [documentHighlightProvider (or/c boolean? jsobj?)]
   [documentSymbolProvider (or/c boolean? jsobj?)]
   [codeActionProvider (or/c boolean? jsobj?)]
   [codeLensProvider jsobj?]
   [documentLinkProvider jsobj?]
   [colorProvider (or/c boolean? jsobj?)]
   [documentFormattingProvider (or/c boolean? jsobj?)]
   [documentRangeFormattingProvider (or/c boolean? jsobj?)]
   [documentOnTypeFormattingProvider jsobj?]
   [renameProvider (or/c boolean jsobj?)]
   [foldingRangeProvider (or/c boolean jsobj?)]
   [executeCommandProvider jsobj?]
   [selectionRangeProvider (or/c boolean? jsobj?)]
   [linkedEditingRangeProvider (or/c boolean? jsobj?)]
   [callHierarchyProvider (or/c boolean? jsobj?)]
   [semanticTokensProvider jsobj?]
   [monikerProvider (or/c boolean? jsobj?)]
   [typeHierarchyProvider (or/c boolean? jsobj?)]
   [inlineValueProvider (or/c boolean? jsobj?)]
   [inlayHintProvider (or/c boolean? jsobj?)]
   [diagnosticProvider jsobj?]
   [workspaceSymbolProvider (or/c boolean? jsobj?)]
   [workspace jsobj?]
   [experimental jsexpr?]))

(define-json-expander RenameOptions
  (#:required)
  (#:optional
   [prepareProvider boolean?]))

(define-json-expander SemanticTokensOptions
  (#:required
   [legend jsobj?])
  (#:optional
   [range (or/c boolean? (and/c dict? dict-empty?))]
   [full (or/c boolean? jsobj?)]))

(define-json-expander SemanticTokensLegend
  (#:required
   [tokenTypes (listof string?)]
   [tokenModifiers (listof string?)])
  (#:optional))

(define-json-expander CodeActionOptions
  (#:required)
  (#:optional
   [codeActionKinds (listof string?)]
   [resolveProvider boolean?]))