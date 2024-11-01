;;; claudia-eglot.el --- Claude AI integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Zacho

;; Author: Martin Zacho <hi@martinzacho.net>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (uuidgen "0.3") (markdown-mode "2.3"))
;; Keywords: ai, tools, productivity, codegen
;; URL: https://github.com/mzacho/claudia

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Claudia is an Emacs integration for the Claude AI
;; assistant.  Claudia is not affiliated with Anthropic or
;; Claude.ai.  By using Claudia you agree to use it at your own risk,
;; and acknowledge potential violation of Anthropic's Terms of
;; Service, for which you assume full responsibility for any
;; consequences.  Understand that Anthropic does not support this tool
;; and please review Anthropic's Terms of Service before using
;; Claudia:
;;
;; https://www.anthropic.com/legal/consumer-terms

;; Code:

(defcustom claudia-eglot-explain-prompt
  "Please explain the meaning of the following code symbol:

Symbol name:

%s

Definition:

%s

Documentation:

%s

Use-sites:

%s

Please provide:
1. A clear explanation of the symbol's purpose
2. Notable patterns in how it's used
3. Any potential gotchas or important considerations
4. Example usage if helpful"
  "Template for code explanation prompts.
The template should contain four %s placeholders for:
1. Name of symbol
2. Documentation string (if available)
3. Code-region containing definition
4. Code-regions of (all/ notable) use-sites"
  :type 'string
  :group 'claudia)

(defcustom claudia-eglot-explain-symbol-xref-context 2
  "How to extract context for `claudia-eglot-explain-symbol-at-point'.
The varible determines the strategy to use for extracting context around symbol
references (use-sites).  It can be either:
- A number indicating how many lines of context to include around the symbol
- The symbol `lsp-symbolprovider' to use LSP's documentSymbol provider.
  to determine the full context range."
  :type '(choice (integer :tag "Lines of context")
                 (const :tag "Use LSP symbol provider" lsp-symbolprovider))
  :group 'claudia)

(defun claudia--eglot-get-definition-xrefs ()
  "Get definition xrefs for symbol at point."
  (eglot--lsp-xrefs-for-method
   :textDocument/definition))

(defun claudia--eglot-get-reference-xrefs ()
  "Get reference xrefs for symbol at point.
Reuses cached references from `eglot--lsp-xref-refs' when available."
  (or eglot--lsp-xref-refs
      (eglot--lsp-xrefs-for-method
       :textDocument/references
       :extra-params `(:context (:includeDeclaration t)))))

(defun claudia--eglot-request-document-symbols ()
  "Request document symbols for current buffer from LSP server."
  (when (eglot--server-capable :documentSymbolProvider)
    (append
     (jsonrpc-request
      (eglot--current-server-or-lose)
      :textDocument/documentSymbol
      `(:textDocument ,(eglot--TextDocumentIdentifier)))
     nil)))

(defun claudia--eglot-get-hover-info ()
  "Get hover information for symbol at point."
  (when (eglot--server-capable :hoverProvider)
    (jsonrpc-request
     (eglot--current-server-or-lose)
     :textDocument/hover
     (eglot--TextDocumentPositionParams))))

(defun claudia--eglot-format-xref-choice (xref root)
  "Format XREF for completion choice relative to project ROOT."
  (let* ((loc (xref-item-location xref))
         (group (xref-location-group loc))
         (file-name (file-relative-name group root)))
    (cons (format "%s (%s)" (xref-item-summary xref) file-name)  xref)))

(defun claudia--eglot-choose-definition (xrefs root)
  "Let user choose from multiple XREFS definitions in project at ROOT."
  (if (= (length xrefs) 1)
      (car xrefs)
    (let* ((items
            (mapcar
             (lambda (xref)
               (claudia--eglot-format-xref-choice xref root))
             xrefs))
           (chosen (completing-read "Choose definition: "
                                    (mapcar #'car items)
                                    nil t)))
      (cdr (assoc chosen items)))))

(defun claudia--eglot-format-context (contexts def-name)
  "Format CONTEXTS and DEF-NAME into prompt arguments.
MULTIPLE-DEFINITIONS-P indicates if user selected from multiple definitions."
  (message "contexts: %s" contexts)
  (let* ((def (cl-find-if (lambda (ctx) (plist-get ctx :is-definition))
                          contexts))
         (def-str (format "Defined in %s:%d:\n```\n%s\n```"
                          (plist-get def :file)
                          (plist-get def :line)
                          (plist-get def :content)))
         (doc-str (if-let ((info (claudia--eglot-get-hover-info)))
                      (eglot--hover-info (plist-get info :contents))
                    "N/A.\n"))
         (ref-str (mapconcat
                   (lambda (ctx)
                     (unless (plist-get ctx :is-definition)
                       (format "\nUsed in %s:%d:\n```\n%s\n```"
                               (plist-get ctx :file)
                               (plist-get ctx :line)
                               (plist-get ctx :content))))
                   (delq nil contexts) "\n")))
    (list def-name def-str doc-str ref-str)))

(defun claudia--eglot-document-symbol-range (sym)
  "Get the start/ end range for LSP provided symbol SYM."
  (let ((range-start (plist-get (plist-get sym :range) :start))
        (range-end   (plist-get (plist-get sym :range) :end)))
    (cons range-start range-end)))

(defun claudia--eglot-find-sym (syms start-pos)
  "Find the symbol from SYMS respresented by START-POS."
  (let* ((get-range-start
          (lambda (sym)
            (eglot--lsp-position-to-point
             (car (claudia--eglot-document-symbol-range sym)))))
         (syms-sorted (cl-sort syms #'< :key get-range-start)))
    (claudia--eglot-find-sym-inner syms-sorted start-pos)))

(defun claudia--eglot-find-sym-inner (syms start-pos)
  "Helper function for `claudia--eglot-find-sym'.
SYMS is a plist with LSP document symbols sorted by starting position, and the
function returns the symbol with the latest starting position before START-POS."
  (if (length< syms 2)
      (car syms)
    (if-let ((next-range-start (car (claudia--eglot-document-symbol-range (cadr syms)))))
        (if (< start-pos (eglot--lsp-position-to-point next-range-start))
            (car syms)
          (claudia--eglot-find-sym (cdr syms) start-pos))
      (claudia--eglot-find-sym (cdr syms) start-pos))))

(defun claudia--eglot-extract-context (xref is-definition root)
  "Return some context on XREF in project at ROOT.
This includes code-region of XRES, which may be a definition if IS-DEFINITION
is non-nil.  Otherwise the code region is a use-site/ referral."
  (let* ((loc (xref-item-location xref))
         (group (xref-location-group loc))
         (marker (xref-location-marker loc))
         (buf (marker-buffer marker))
         (start-point (marker-position marker)))
    (with-current-buffer buf
      (save-excursion
        (let ((region
               ;; Resolve code-region around xref: If `is-definition' is t or the
               ;; `claudia-eglot-explain-symbol-xref-context' is 'lsp-symbolprovider
               ;; try to use LSP symbolProvider to find the relevant region.
               (if-let*
                   ((use-lsp (or is-definition
                                 (eq claudia-eglot-explain-symbol-xref-context
                                     'lsp-symbolprovider)))
                    (symbols (claudia--eglot-request-document-symbols))
                    (sym (claudia--eglot-find-sym symbols start-point))
                    (pos-start (car (claudia--eglot-document-symbol-range sym)))
                    (pos-end   (cdr (claudia--eglot-document-symbol-range sym))))
                   (cons (eglot--lsp-position-to-point pos-start)
                         (eglot--lsp-position-to-point pos-end))
                 ;; Use hard-coded context around use-sites
                 (goto-char start-point)
                 (let ((lines (if (numberp claudia-eglot-explain-symbol-xref-context)
                                  claudia-eglot-explain-symbol-xref-context
                                2)))
                   (cons (line-beginning-position (- 1 lines))
                         (line-end-position (+ lines 1)))))))
          (list :file (file-relative-name group root)
                :content (buffer-substring-no-properties
                          (car region)
                          (cdr region))
                :is-definition is-definition
                :line (line-number-at-pos marker)
                :summary (xref-item-summary xref)))))))


(defun claudia-eglot-explain-symbol-at-point ()
  "Explain the symbol at point using Eglot and Claude."
  (interactive)
  (unless (eglot-current-server)
    (user-error "Eglot is not active in this buffer"))
  (let* ((server (eglot--current-server-or-lose))
         (root (project-root (eglot--project server)))
         ;; Get location of definition and use sites
         (def-xrefs (claudia--eglot-get-definition-xrefs))
         (chosen-def (claudia--eglot-choose-definition def-xrefs root))
         (ref-xrefs (claudia--eglot-get-reference-xrefs))
         ;; Extract context with code-regions for each xref
         (contexts
          (cons
           (claudia--eglot-extract-context chosen-def t root)
           (mapcar (lambda (xref)
                     (claudia--eglot-extract-context xref nil root))
                   ref-xrefs)))
         ;; Format prompt
         (def-name (substring-no-properties (xref-match-item-summary chosen-def)))
         (prompt-args (claudia--eglot-format-context contexts def-name))
         (prompt (apply #'format claudia-eglot-explain-prompt prompt-args)))

    (if claudia-debug
        (message  "ref: %s, prompt: %s" chosen-def prompt))

    (claudia-create-chat
     (format "[code-explain: %s]"
             (plist-get
              (cl-find-if
               (lambda (ctx) (plist-get ctx :is-definition))
               contexts)
              :summary)))
    (claudia-query prompt t)))

(provide 'claudia-eglot)

;; claudia-eglot ends here
