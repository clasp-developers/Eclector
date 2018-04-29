(cl:defpackage #:eclector.reader
  (:use #:common-lisp)
  ;; When the reader is compiled for the purpose of cross compilation,
  ;; we must shadow a certain number of symbols that would otherwise
  ;; clash with the corresponding symbols in the host package
  ;; COMMON-LISP.
  (:shadow
   #:read
   #:read-preserving-whitespace)
  ;; Contrary to other variables affecting the reader, we cannot use
  ;; the host version of *READTABLE* because we do not necessarily
  ;; use the same representation of readtables as the host does, and
  ;; Common Lisp does not have a standardized API for manipulating
  ;; readtables.  Perhaps we should write a CDR (Common Lisp Document
  ;; Repository) document suggesting such an API.
  (:shadowing-import-from #:eclector.readtable
   #:*readtable*)

  (:import-from #:eclector.base
   #:%reader-error

   #:stream-position-reader-error
   #:stream-position)

  (:import-from #:eclector.readtable
   #:unknown-macro-sub-character)

  (:export
   #:readtable
   #:*readtable*
   #:*client*
   #:*skip-reason*
   #:read
   #:read-preserving-whitespace
   #:*preserve-whitespace*
   #:read-common
   #:read-token
   #:note-skipped-input
   #:interpret-token
   #:interpret-symbol
   #:call-reader-macro
   #:fixup
   ;; Backquote customization.
   #:wrap-in-quasiquote
   #:wrap-in-unquote
   #:wrap-in-unquote-splicing
   ;; Names of additional conditions.
   #:backquote-condition
   #:invalid-context-for-backquote
   #:comma-not-inside-backquote
   #:object-must-follow-comma
   #:unquote-splicing-in-dotted-list
   #:unquote-splicing-at-top
   #:invalid-context-for-consing-dot
   #:object-must-follow-consing-dot
   #:multiple-objects-following-consing-dot
   #:invalid-context-for-right-parenthesis

   #:symbol-name-must-not-end-with-package-marker
   #:symbol-does-not-exist
   #:symbol-is-not-external
   #:two-package-markers-must-be-adjacent
   #:two-package-markers-must-not-be-first
   #:symbol-can-have-at-most-two-package-markers
   #:uninterned-symbol-must-not-contain-package-marker

   #:unknown-macro-sub-character
   #:numeric-parameter-supplied-but-ignored
   #:numeric-parameter-not-supplied-but-required

   #:unknown-character-name
   #:digit-expected
   #:invalid-radix
   #:invalid-default-float-format
   #:too-many-elements
   #:no-elements-found
   #:incorrect-initialization-length
   #:single-feature-expected
   #:sharpsign-invalid
   #:sharpsign-equals-label-defined-more-than-once
   #:sharpsign-sharpsign-undefined-label
   ;; Names of macros related to backquote.
   ;; We export them so that the pretty printer
   ;; can use them properly.
   #:quasiquote #:unquote #:unquote-splicing))
