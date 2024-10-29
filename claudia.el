;;; claudia.el --- Claude AI integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Zacho

;; Author: Martin Zacho <hi@martinzacho.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (uuidgen "0.3") (markdown-mode "2.3"))
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
;;
;; Todos:
;;
;; - defcustom magit-commit cleanup chats (flag)
;; - chat-lists: show percentage of project knowledge used

;;; Code:


(require 'markdown-mode)
(require 'tabulated-list)
(require 'uuidgen)
(require 'url)
(require 'json)

(defvar claudia-gh-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'claudia-suggest-commit-msg)
    (define-key map (kbd "p") 'claudia-gh-summarize-pr-from-url)
    map)
  "Keymap for Claudia GitHub-related commands.")

(defvar claudia-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'claudia-create-project)
    (define-key map (kbd "c") 'claudia-create-chat)
    (define-key map (kbd "k") 'claudia-clear-context)
    (define-key map (kbd "q") 'claudia-query)
    (define-key map (kbd "l") 'claudia-list-chats)
    (define-key map (kbd "s") 'claudia-send-visiting-buffer)
    (define-key map (kbd "e") 'claudia-explain-region)
    (define-key map (kbd "w") 'claudia-summarize-page-from-url)
    (define-key map (kbd "g") claudia-gh-map)
    map)
  "Global keymap for Claudia integration functions.")

(defgroup claudia nil
  "Customization group for Claude integration."
  :group 'external)

(defcustom claudia-debug nil
  "Enable debug printing."
  :type 'boolean
  :group 'claudia)

(defcustom claudia-api-key nil
  "Claude session key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'claudia)

(defcustom claudia-api-url "https://api.claude.ai/api"
  "Base URL for Claude API."
  :type 'string
  :group 'claudia)

(defcustom claudia-organization-id nil
  "Claude organization ID."
  :type 'string
  :group 'claudia)

(defcustom claudia-model "claude-3-5-sonnet-20240620"
  "The model to use for new chats.  Possible values are:

- claude-3-5-sonnet-20450620
- claude-3-opus-20240229
- claude-3-haiku-2024030"
  :type 'string
  :group 'claudia)

(defcustom claudia-gh-program "gh"
  "Path to the GitHub CLI executable."
  :type 'string
  :group 'claudia)

(defcustom claudia-download-url-program "lynx"
  "Path to program to use for downloading urls."
  :type 'string
  :group 'claudia)

(defcustom claudia-download-url-program-args "-dump -nolist -width=1000"
  "Path to args to pass to `claudia-download-url-program'."
  :type 'string
  :group 'claudia)

(defcustom claudia-explain-include-context t
  "Wether to include code context around around `claudia-explain-region' queries."
  :type 'boolean
  :group 'claudia)

(defcustom claudia-explain-context-len 250
  "Lines of context to include around region for `claudia-explain-region'."
  :type 'string
  :group 'claudia)

(defvar claudia--current-project-config nil
  "Alist to store project-level configuration for Claudia.
Currently this includes name, id and instructions.")

(defvar claudia--current-chat nil
  "Current Claudia chat conversation.")

(defvar claudia--query-display-response-buf t
  "Display the reponse buffer after a calling `claudia-query-async'.")

(defun claudia-get-api-key ()
  "Retrieve the Claude API key."
  (if (functionp claudia-api-key)
      (funcall claudia-api-key)
    claudia-api-key))

(defun claudia--set-project-config (key value)
  "Set a project-level configuration KEY to VALUE."
  (setq claudia--current-project-config
        (cons (cons key value)
              (assq-delete-all key claudia--current-project-config))))

(defun claudia-get-project-config (key)
  "Get the value of a project-level configuration KEY."
  (alist-get key claudia--current-project-config))

(defun claudia--set-initial-instruction ()
  "Set the initial instruction for Claude AI interactions."
  (claudia--set-project-config
   'initial-instruction
   "For this entire conversation, please skip any introductory
   paragraphs or context restatements in your responses. Be concise
   and focus on the technical details. Never repeat identical code
   from previous responses. Don't apologize for any confusion."))

(defun claudia--set-markdown-config ()
  "Instruct Cladia to respond with valid markdown."
  (claudia--set-project-config 'markdown-mode t)
  (claudia--set-project-config
   'markdown-instruction
   "For this entire conversation, please format all your responses in
   valid Markdown. This includes:

- Using proper heading levels (# for main headings, ## for
- subheadings, etc.)  Correctly formatting lists (both ordered and
- unordered) Using backticks for inline code and triple backticks for
- code blocks Properly formatting links, bold, and italic text
- Separating paragraphs with blank lines Using blockquotes where
- appropriate"))

(defun claudia--set-last-instruction ()
  "Set last instruction (confirmation) to Claude before starting a new chat."
  (claudia--set-project-config
   'last-instruction
   "Please confirm you understand and will follow these instructions."))

(defun claudia--current-project-instructions ()
  "Instructions to Claudia when starting a new chat in the current project."
  (mapconcat
   #'append
   (list (claudia-get-project-config 'initial-instruction)
         (claudia-get-project-config 'markdown-instruction)
         (claudia-get-project-config 'last-instruction))
   "\n\n"))

(defun claudia--explain-region-instruction (region major-mode-name file-name &optional context)
  "The instructions given to Claude when running `claudia-explain-region'.
The instruction asks for an exlpanaiotn of code REGION in FILE-NAME using
MAJOR-MODE-NAME, optionnaly including code CONTEXT around the code region."
  (format
   "Please explain this code:

Code context:
- File: %s
- Major mode: %s

Surrounding context:
```
%s
```

Code to explain:
```
%s
```

Please provide a detailed explanation of what this code does, any
notable patterns or idioms used, and potential improvements or
considerations."
   (or file-name "N/A")
   major-mode-name
   (or context "N/A")
   region))

(defun claudia--magit-commit-msg-instruction ()
  "The instructions given to Claude when running `claudia-suggest-commit-msg'."
  "Please suggest a concise and informative commit message based on the diff I
just put in your knowledge context. The message should follow this format:

<type>[optional scope]: <description>

[optional body]

The commit type can include the following:

- feat – a new feature is introduced with the changes
- fix – a bug fix has occurred
- chore – changes that do not relate to a fix or feature
and don't modify src or test files (for example updating dependencies)
- refactor – refactored code that neither fixes a bug nor adds a feature
- docs – updates to documentation such as a the README or other markdown
files
- style – changes that do not affect the meaning of the code,
likely related to code formatting such as white-space, missing
semi-colons, and so on.
- test – including new or correcting previous
tests
- perf – performance improvements
- ci – continuous integration
related build – changes that affect the build system or external
dependencies
- revert – reverts a previous commit The commit type
subject line should be all lowercase with a character limit to
encourage succinct descriptions.

The optional commit body should be used to provide further detail that
cannot fit within the character limitations of the subject line
description.

Provide the RAW COMMIT MESSAGE WITHOUT ANY ADDITIONAL TEXT.  Think
really hard about the `magit-diff' in your knowledge context.")

(defun claudia--gh-summarize-pr-instruction ()
  "The prompt for `claudia-gh-summarize-pr-from-url'."
  "Please provide a concise summary of the changes in this pull request. Include:
1. The title of the PR
1. The main purpose or goal of the changes
2. Key files or components modified
3. Any notable additions, deletions, or modifications
4. Potential impact or implications of these changes

Please be brief but informative, focusing on the technical details and
the most important aspects of the diff.")

(defun claudia--web-summary-instruction (url)
  "The prompt for `claudia-summarize-page-from-url' for summarizing URL."
  (format "Please provide a concise, but rather deep, summary of the
  web page content from %s. Include the main topics, key points, and
  any significant information." url))

(defun claudia-api-request (method endpoint &optional data)
  "Make a request to Claude using HTTP METHOD, API ENDPOINT and request body DATA."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,(claudia-get-api-key))
            ("Cookie" . ,(format "sessionKey=%s" (claudia-get-api-key)))))
         (url-request-data (when data (encode-coding-string (json-encode data) 'utf-8)))
         (response-buffer (url-retrieve-synchronously
                           (concat claudia-api-url endpoint)
                           nil nil 30)))
    (with-current-buffer response-buffer
      (if claudia-debug
          (message "API response: %s" (buffer-string)))
      ;; delete HTTP header from response
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point-min) (point))
      (condition-case nil (json-read)
        (error (message "Failed to parse JSON response"))))))

(defun claudia-create-project (name description)
  "Create a new project with NAME and DESCRIPTION.
The projec is set as the current working project, so any new chat started with
`claudia-create-chat' or knowledge context added with
`claudia-send-visiting-buffer' will be within this project."
  (interactive "sEnter project name: \nsEnter project description: ")
  (let* ((response (claudia-api-request
                    "POST"
                    (format "/organizations/%s/projects" claudia-organization-id)
                    `(("name" . ,name)
                      ("description" . ,description)
                      ("is_private" . t))))
         (project-id (alist-get 'uuid response)))
    (if project-id
        (progn
          (claudia--set-project-config 'id project-id)
          (claudia--set-project-config 'name name)
          (claudia--set-markdown-config)
          (claudia--set-initial-instruction)
          (claudia--set-last-instruction)
          (message "Created and set current project to '%s' (ID: %s)" name project-id))
      (message "Failed to create project"))))

(defun claudia-send-visiting-buffer ()
  "Add the content of the visiting buffer to the current project."
  (interactive)
  (if claudia--current-project-config
      (let* ((file-name (buffer-name))
             (content (buffer-string))
             (project-id (claudia-get-project-config 'id))
             (project-name (claudia-get-project-config 'name))
             (response (claudia-api-request
                        "POST"
                        (format "/organizations/%s/projects/%s/docs" claudia-organization-id project-id)
                        `(("file_name" . ,file-name)
                          ("content" . ,content)))))
        (if (alist-get 'uuid response)
            (message "Added buffer content to project '%s'" project-name)
          (message "Failed to add buffer content to project")))
    (message "No current project config set. Use claudia-create-project first.")))


(defun claudia-explain-region ()
  "Send the active region to Claude with context and ask for an explanation."
  (interactive)
  (if (use-region-p)
      (let* ((region-content (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
             (major-mode-name (symbol-name major-mode))
             (file-name (buffer-file-name))
             (context (if claudia-explain-include-context
                          (buffer-substring-no-properties
                           (max (point-min) (- (region-beginning) claudia-explain-context-len))
                           (min (point-max) (+ (region-end) claudia-explain-context-len)))))
             (query (claudia--explain-region-instruction
                     region-content major-mode-name file-name context)))
        (deactivate-mark)
        (claudia-create-chat
         (format "explain: %s..." (substring region-content 0 (min 30 (length region-content)))))
        (claudia--query-async query t t))
    (message "No active region. Please select some code to explain.")))

(defun claudia-clear-context ()
  "Delete all project knowledge files from the current project."
  (interactive)
  (if claudia--current-project-config
      (let* ((project-id (claudia-get-project-config 'id))
             (project-name (claudia-get-project-config 'name))
             (files (claudia-api-request
                     "GET"
                     (format "/organizations/%s/projects/%s/docs" claudia-organization-id project-id))))
        (if files
            (when (yes-or-no-p (format "Are you sure you want to delete all files from project '%s'? " project-name))
              (let ((deleted-count 0)
                    (total-files (length files)))
                (dolist (file (append files nil))
                  (let* ((file-uuid (alist-get 'uuid file))
                         (file-name (alist-get 'file_name file))
                         (delete-response (claudia-api-request
                                           "DELETE"
                                           (format "/organizations/%s/projects/%s/docs/%s"
                                                   claudia-organization-id project-id file-uuid))))
                    (if delete-response
                        (progn
                          (setq deleted-count (1+ deleted-count))
                          (message "Deleted file: %s (%d/%d)" file-name deleted-count total-files))
                      (message "Failed to delete file: %s" file-name))))
                (message "Deleted %d out of %d files from project '%s'"
                         deleted-count total-files project-name)))
          (message "No files found in the current project.")))
    (message "No current project set. Use claudia-create-project first.")))

(defun claudia-create-chat (name)
  "Create a new chat conversation with NAME in the current project.
The chate is set as the current chat."
  (interactive "sEnter chat name: \n")
  (unless claudia--current-project-config
    (claudia-create-project "no name" "no desc"))
  (let* ((project-id (claudia-get-project-config 'id))
         (response (claudia-api-request
                    "POST"
                    (format "/organizations/%s/chat_conversations" claudia-organization-id)
                    `(("uuid" . ,(format "%s" (uuidgen-4)))
                      ("name" . ,name)
                      ("project_uuid" . ,project-id)
                      ("model" . ,claudia-model)))))
    (if (alist-get 'uuid response)
        (let ((id (alist-get 'uuid response))
              (first-instructions (claudia--current-project-instructions)))
          (setq claudia--current-chat
                `((id . ,id)
                  (name . ,name)))
          (when first-instructions
            (with-current-buffer (claudia--get-chat-buffer)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (claudia--query-async first-instructions))))
          (message "Created chat conversation '%s' in project '%s' (ID: %s)"
                   name (claudia-get-project-config 'name) id))
      (message "Failed to create chat conversation"))))

(defun claudia-query (prompt &optional arg)
  "Send PROMPT to Claude in the current chat conversation.
With prefix ARG, don't display the prompt in the *claudia-chat* buffer.
When called interactively, prompts for the query string."
  (interactive "sclaudia query: \nP")
  (unless claudia--current-chat
    (claudia-create-chat "no name"))
  (if arg
      (claudia--query-async prompt claudia--query-display-response-buf t)
    (claudia--query-async prompt claudia--query-display-response-buf t t)))

(defun claudia--fmt-markdown-prompt (sender time &optional prompt)
  "Format the line displayed in `claudia-chat' for SENDER's PROMPT at TIME."
  (let ((beginning (if (= (point-min) (point-max)) "" "\n"))
        (time-fmt (format-time-string "%F %T" time)))
    (if prompt
        ;; for some reason Claude responses start with an annoying whitespace
        (let ((prompt-fmt (if (string= sender "Claude") (substring prompt 1) prompt)))
          (format "%s**%s**: (%s)\n%s\n" beginning sender time-fmt prompt-fmt))
      (format "%s**%s**: (%s)\n" beginning sender time-fmt))))

(defun claudia--query-async (prompt &optional display-response-buf show-response show-prompt callback)
  "Send PROMPT to Claude in current chat conversation in an async web request.
If DISPLAY-RESPONSE-BUF is non-nil display the *claudia-chat* buffer when the
response arrives.  If SHOW-RESPONSE is non-nil show the streaming response in
the *claudia-chat* buffer, and include the prompt if SHOW-PROMPT is also
non-nil.

If CALLBACK and SHOW-RESPONSE are non-nil then CALLBACK is called when the
response is ready with a single string argument (that is, the response from
Claude)."
  (if claudia--current-chat
      (let* ((chat-id (alist-get 'id claudia--current-chat))
             (url (concat claudia-api-url
                          (format "/organizations/%s/chat_conversations/%s/completion"
                                  claudia-organization-id chat-id)))
             (url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("Cookie" . ,(format "sessionKey=%s" (claudia-get-api-key)))
                ("Accept" . "text/event-stream")))
             (url-request-data
              (encode-coding-string
               (json-encode
                `(("prompt" . ,prompt)
                  ("timezone" . "UTC")
                  ("attachments" . [])
                  ("files" . [])))
               'utf-8))
             (response-buffer (claudia--get-chat-buffer))
             (parse-buffer (generate-new-buffer "*claudia-sse-parse*")))
        (if show-response
            (let ((prompt-point
                   (with-current-buffer response-buffer
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (if show-prompt
                           (insert (claudia--fmt-markdown-prompt
                                    "You" (current-time) prompt)))
                       (point)))))

              (url-retrieve
               url
               (lambda (status
                   prompt-point
                   parse-buffer
                   response-buffer
                   display-response-buf
                   callback)
                 (if claudia-debug
                     (message "API response: %s" (buffer-string)))
                 (if (plist-get status :error)
                     (error "Error: %s" (plist-get status :error))
                   (if display-response-buf
                       (display-buffer response-buffer))
                   (set-buffer-multibyte t)
                   (goto-char (point-min))
                   (re-search-forward "^$")
                   (forward-line)
                   (delete-region (point-min) (point))
                   (set-buffer-multibyte t)
                   (let ((response-point
                          (with-current-buffer response-buffer
                            (let ((inhibit-read-only t))
                              (insert (claudia--fmt-markdown-prompt "Claude" (current-time)))
                              (point)))))
                     (claudia--parse-sse parse-buffer response-buffer)
                     (with-current-buffer response-buffer
                       (let ((inhibit-read-only t))
                         (insert "\n"))
                       (goto-char prompt-point)
                       (forward-line 2)
                       (message nil)
                       (if display-response-buf
                           (recenter-top-bottom 0))
                       (let ((response (buffer-substring-no-properties
                                        response-point (point-max))))
                         (when (functionp callback)
                           (funcall callback response)))))))
               `(,prompt-point
                 ,parse-buffer
                 ,response-buffer
                 ,display-response-buf
                 ,callback))
              (message "waiting for claude..."))
          (url-retrieve
           url
           (lambda (status)
             (if (plist-get status :error)
                 (message "Error: %s" (plist-get status :error))))))
        (kill-buffer parse-buffer))

    (message "No current chat conversation. Use claudia-create-chat first.")))

(defun claudia--parse-sse (parse-buffer response-buffer &optional rec)
  "Parse SSE data in PARSE-BUFFER and update RESPONSE-BUFFER.
The optional argument REC is set when recursing, and used to distinguish the
first call to the function, since we want to strip the first character from the
response, which is apparently always a whitespace."
  (let ((mark (point-min))
        (mark-response
         (with-current-buffer response-buffer
           (point-max))))
    (while (not (eobp))
      (when (looking-at "data: ")
        (forward-char 6)
        (let* ((json-object-type 'plist)
               (json-key-type 'keyword)
               (data (json-read-from-string (buffer-substring-no-properties (point) (line-end-position))))
               (completion (plist-get data :completion)))
          (when completion
            (with-current-buffer response-buffer
              (goto-char (point-max))
              (let ((inhibit-read-only t))
                (insert completion))))))
      (forward-line))
    (if (not rec)
        ;; delete the first space in Claude's response. This is
        ;; otherwise stopping markdown headings from being rendered ;(
        (with-current-buffer response-buffer
          (let ((inhibit-read-only t))
            (goto-char mark-response)
            (delete-char 1)
            (goto-char (point-max)))))
    (when (< mark (point))
      (delete-region mark (point)))
    (goto-char (point-max))
    (accept-process-output nil 0.1)
    (when (not (eobp))
      (claudia--parse-sse parse-buffer response-buffer t))))

;; chat list

(defvar claudia-chat-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'claudia-chat-list-select-chat)
    (define-key map (kbd "d") 'claudia-chat-list-mark-delete)
    (define-key map (kbd "u") 'claudia-chat-list-unmark)
    (define-key map (kbd "x") 'claudia-chat-list-execute)
    (define-key map (kbd "r") 'claudia-chat-list-refresh)
    (define-key map (kbd "c") 'claudia-create-chat)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "s") 'claudia-chat-list-toggle-sorting-mode)
    map)
  "Keymap for `claudia-chat-list-mode'.")

(define-derived-mode claudia-chat-list-mode tabulated-list-mode "claudia:chats"
  "Major mode for listing Claude chat conversations."
  (let* ((window-width (- (window-width) 6)) ; Subtract 4 for some padding
         (name-width (floor (* 0.35 window-width)))
         (updated-width 20)
         (created-width 20)
         (project-width (- window-width name-width updated-width created-width)))
    (setq tabulated-list-format `[("Name" ,name-width t)
                                  ("Project" ,project-width t)
                                  ("Last Updated" ,updated-width t)
                                  ("Created" ,created-width t)]))
  (setq tabulated-list-sort-key (cons "Last Updated" t))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-use-header-line t)
  (add-hook 'tabulated-list-revert-hook #'claudia-chat-list-refresh nil t)
  (tabulated-list-init-header))

(defvar claudia-chat-list-sorting-modes
  '(("Name" . 0)
    ("Project" . 1)
    ("Last Updated" . 2)
    ("Created" . 3))
  "List of sorting modes for claudia-chat-list.")

(defvar claudia-chat-list-sorting-mode "Last Updated"
  "The current sorting mode for claudia-chat-list.")

(defun claudia-chat-list-toggle-sorting-mode ()
  "Toggle the current sorting mode for claudia-chat-list.
Sorting modes are: Name, Project, Last Updated, and Messages."
  (interactive)
  (let* ((modes (mapcar #'car claudia-chat-list-sorting-modes))
         (next (or (cadr (member claudia-chat-list-sorting-mode modes))
                   (car modes))))
    (setq claudia-chat-list-sorting-mode next
          tabulated-list-sort-key (cons next t)))
  (tabulated-list-print)
  (tabulated-list-init-header)
  (message "Sorting by %s" claudia-chat-list-sorting-mode))

(defun claudia-list-chats ()
  "Display a list of Claude chat conversations."
  (interactive)
  (let ((buffer (get-buffer-create "*claudia-chat-list*")))
    (with-current-buffer buffer
      (claudia-chat-list-mode)
      (claudia-chat-list-refresh))
    (switch-to-buffer buffer)))

(defun claudia-chat-list-refresh ()
  "Refresh the list of chat conversations."
  (interactive)
  (let ((response (claudia-api-request
                   "GET"
                   (format "/organizations/%s/chat_conversations" claudia-organization-id))))
    (if (and (listp response) (string= (alist-get 'type response) "error"))
        (message "Failed to refresh chat list: %s" response)
      (progn
        (if claudia-debug
            (message response))
        (setq tabulated-list-entries
              (mapcar
               (lambda (chat)
                 (let* ((id (alist-get 'uuid chat))
                        (name (alist-get 'name chat))
                        (updated-at (alist-get 'updated_at chat))
                        (created-at (alist-get 'created_at chat))
                        (project (alist-get 'project chat))
                        (project-name (if project (alist-get 'name project) "[no project]"))
                        (project-id (if project (alist-get 'uuid project) "[no project]"))
                        (updated-at-fmt (if updated-at
                                            (format-time-string
                                             "%F %T"
                                             (date-to-time updated-at))
                                          "nil"))
                        (created-at-fmt (if created-at
                                            (format-time-string
                                             "%F %T"
                                             (date-to-time created-at))
                                          "nil"))
                        (project-name-with-id (propertize project-name 'project-id project-id)))
                   `(,id [,name ,project-name-with-id ,updated-at-fmt ,created-at-fmt])))
               response))))
    (tabulated-list-print t)
    (message "refreshed chat list")))

(defun claudia-chat-list-select-chat ()
  "Select the chat at point in the chat list."
  (interactive)
  (claudia--switch-to-chat t))


(defun claudia--switch-to-chat (display-buffer)
  "Switch to the chat at the current tabulated list entry.
If DISPLAY-BUFFER is non-nil display the *claudia-chat* buffer."
  (let* ((chat (tabulated-list-get-entry))
         (chat-id (tabulated-list-get-id))
         (chat-name (elt chat 0))
         (project-name (elt chat 1))
         (project-id (get-text-property 0 'project-id project-name))
         (chat-history (claudia--fetch-chat-history chat-id)))
    (setq claudia--current-chat `((id . ,chat-id) (name . ,chat-name)))
    (claudia--set-project-config 'id project-id)
    (claudia--set-project-config 'name project-name)
    ;; todo: project's initial instruction is hardcoded. figure out a
    ;; way to set this, either by reading it from a
    ;; .emacs.d/claude-state file on disk, or fetching it from the
    ;; conversation somehow.
    (claudia--set-markdown-config)
    (claudia--set-initial-instruction)
    (claudia--set-last-instruction)
    (claudia--update-chat-buffer chat-history)
    (message "Switched to chat '%s' in project '%s'" chat-name project-name)
    (if display-buffer (claudia--get-chat-buffer t))))


(defvar claudia-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'claudia-query)
    (define-key map (kbd "q") 'claudia-query)
    (define-key map (kbd "e") 'claudia-explain)
    (define-key map (kbd "c") 'claudia-create-chat)
    (define-key map (kbd "k") 'claudia-clear-context)
    (define-key map (kbd "n") 'claudia--chat-next-prompt)
    (define-key map (kbd "p") 'claudia--chat-previous-prompt)
    (define-key map (kbd "l") 'claudia-list-chats)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `claudia-chat-mode'.")

(define-derived-mode claudia-chat-mode markdown-mode "claudia:chat"
  "Major mode used for displaying Claude chat sessions."
  (read-only-mode))

(defvar claudia--chat-prompt-regex "^\\*\\*\\(You\\)\\|\\(Claude\\)\\*\\*: ")

(defun claudia--chat-next-prompt ()
  "Move to the next prompt in the Claude chat buffer."
  (interactive)
  (forward-line)
  (if (search-forward-regexp claudia--chat-prompt-regex nil t)
      (beginning-of-line)
    (message "No more prompts found.")))

(defun claudia--chat-previous-prompt ()
  "Move to the previous prompt in the Claude chat buffer."
  (interactive)
  (if (search-backward-regexp claudia--chat-prompt-regex nil t)
      (beginning-of-line)
    (message "No previous prompts found.")))

(defun claudia--get-chat-buffer (&optional display-buf)
  "Get or create the *claudia-chat* buffer for the current chat.
Optionally display it if DISPLAY-BUF is non-nil."
  (with-current-buffer (get-buffer-create "*claudia-chat*")
    (claudia-chat-mode)
    (goto-char (point-max))
    (if display-buf (display-buffer (current-buffer)))
    (current-buffer)))

(defun claudia-chat-list-mark-delete ()
  "Mark a chat for deletion."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun claudia-chat-list-unmark ()
  "Unmark a chat."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun claudia-chat-list-execute ()
  "Execute the marked actions (delete marked chats)."
  (interactive)
  (let (to-delete)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((tag (char-after)))
          (when (eq tag ?D)
            (push (tabulated-list-get-id) to-delete)))
        (forward-line 1)))
    (when to-delete
      (if (yes-or-no-p (format "Delete %d marked chat(s)?" (length to-delete)))
          (progn
            (dolist (id to-delete)
              (claudia-delete-chat id))
            (claudia-chat-list-refresh))
        (message "Deletion cancelled")))))

(defun claudia-delete-chat (id)
  "Delete a chat with the given ID."
  (let ((response (claudia-api-request
                   "DELETE"
                   (format "/organizations/%s/chat_conversations/%s" claudia-organization-id id))))
    (if (and (listp response) (string= (alist-get 'type response) "error"))
        (message "Failed to delete chat: %s" response)
      (message "Chat deleted successfully"))))

(defun claudia--fetch-chat-history (chat-id)
  "Fetch the chat history for the given chat CHAT-ID."
  (claudia-api-request
   "GET"
   (format "/organizations/%s/chat_conversations/%s"
           claudia-organization-id chat-id)))

(defun claudia--update-chat-buffer (chat)
  "Update the *claudia-chat* buffer with the given CHAT."
  (with-current-buffer (claudia--get-chat-buffer)
    (let ((inhibit-read-only t)
          (messages (append (alist-get 'chat_messages chat) nil)))
      (erase-buffer)
      (dolist (message messages)
        (let ((sender (alist-get 'sender message))
              (text (alist-get 'text message))
              (created-at (alist-get 'created_at message)))
          (insert (claudia--fmt-markdown-prompt
                   (if (string= sender "human") "You" "Claude")
                   (date-to-time created-at)
                   text)))))))


(defun claudia-summarize-page-from-url (url)
  "Summarize the contents of the web page at URL using Claude AI."
  (interactive "sEnter URL to summarize: ")
  (let* ((summary-buffer (generate-new-buffer "*claudia-web-summary*"))
         (command (format "%s %s %s" claudia-download-url-program
                          claudia-download-url-program-args url))
         (exit-code (call-process-shell-command command nil summary-buffer)))
    (if (= exit-code 0)
        (progn
          (claudia-create-project
           "[web-summary]"
           "temporary project for web page summary")
          (claudia-create-chat (format "[web-summary: %s]" url))
          (with-current-buffer summary-buffer
            (claudia-send-visiting-buffer))
          (claudia--query-async
           (claudia--web-summary-instruction url) t t))
      (message "could not download url"))
    (kill-buffer summary-buffer)))

(defun claudia-gh-summarize-pr-from-url (url)
  "Summarize a GitHub pull request from its URL."
  (interactive "sEnter url: ")
  (if (string-match "https://github.com/\\([^/]+\\)/\\([^/]+\\)/pull/\\([0-9]+\\)" url)
      (let ((owner (match-string 1 url))
            (repo (match-string 2 url))
            (pr-id (string-to-number (match-string 3 url))))
        (claudia--gh-summarize-pr owner repo pr-id))
    (error "Invalid GitHub pull request URL format")))

(defun claudia--gh-summarize-pr (owner repo pr)
  "Summarize the Gitub pull-request at www.github.com/OWNER/REPO/pull/PR.
Fetches the diff for pull request with ID using the GitHub CLI, sends it to
Claude, and returns a summary of the changes.  Displays an error with the
command output if the GitHub CLI command fails."
  (let* ((diff-command (format "%s pr diff %s -R %s/%s" claudia-gh-program pr owner repo))
         (view-command (format "%s pr view %s -R %s/%s" claudia-gh-program pr owner repo))
         (diff-buffer (generate-new-buffer "*gh-pr-diff*"))
         (view-buffer (generate-new-buffer "*gh-pr-view*"))
         (summary-buffer (generate-new-buffer "*claudia-pr-summary*"))
         (exit-code-diff (call-process-shell-command diff-command nil diff-buffer))
         (exit-code-view (call-process-shell-command view-command nil view-buffer)))
    (if (and (= exit-code-diff 0) (= exit-code-view 0))
        (progn
          (claudia-create-project (format "[PR-%s]" pr) "Temporary project for PR summary")
          (claudia-create-chat (format "[PR-%s-summary]" pr))
          (with-current-buffer diff-buffer
            (claudia-send-visiting-buffer))
          (with-current-buffer view-buffer
            (claudia-send-visiting-buffer))
          (claudia--query-async
           (claudia--gh-summarize-pr-instruction) t t))
      (let (;; most likely each command succeed iff the other succeeds,
            ;; so just take the error from the diff command
            (error-output (with-current-buffer diff-buffer
                            (buffer-string))))
        (message "Error: gh failed: %s" error-output)
        (kill-buffer summary-buffer)))
    (kill-buffer diff-buffer)))


(defun claudia-suggest-commit-msg ()
  "Generate commit message for the currently active `magit-diff' buffer.
Call this function interactively when viewing a git diff in magit.
The suggested commit message will be available in the kill ring,
ready to be pasted into the commit message buffer.

The function will raise an error if:
- No `magit-diff' buffers are found.
- Multiple `magit-diff' buffers are open simultaneously.

This function performs the following steps:
1. Creates a temporary Claude project and chat for commit message generation.
2. Identifies the active `magit-diff' buffer.
3. Sends the diff content to Claude AI.
4. Retrieves the AI-generated commit message suggestion.
5. Copies the suggestion to the kill ring."
  (interactive)
  (claudia-create-project "[commit-msg]" "")
  (claudia-create-chat "[commit-msg]")
  (let ((diff-buffers
         (seq-filter
          (lambda (buf)
            (string-prefix-p "magit-diff" (buffer-name buf)))
          (buffer-list))))
    (cond
     ((null diff-buffers)
      (error "No magit-diff buffers found"))
     ((> (length diff-buffers) 1)
      (error "Multiple magit-diff buffers open.  Please close all but one"))
     (t
      (with-current-buffer (car diff-buffers)
        (claudia-send-visiting-buffer)
        (claudia--query-async
         (claudia--magit-commit-msg-instruction) nil t nil
         (lambda (response)
           (kill-new response)
           (message "claudia: commit message suggestion copied to kill ring" ))))))))



(provide 'claudia)

(provide 'claudia)

;;; claudia.el ends here
