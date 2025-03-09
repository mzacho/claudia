;;; claudia.el --- Claude AI integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Zacho

;; Author: Martin Zacho <hi@martinzacho.net>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (json "1.5") (uuidgen "0.3") (markdown-mode "2.3"))
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

(require 'json)
(require 'tabulated-list)
(require 'url)
(require 'uuidgen)
(require 'markdown-mode)

;; instructions

;;; Code:

(defcustom claudia-anthropic-api-url "https://api.anthropic.com/v1"
  "Base URL for Anthropi API."
  :type 'string
  :group 'claudia)

(defcustom claudia-anthropic-api-key nil
  "Anthropic API key as a string."
  :type 'string
  :group 'claudia)

(defcustom claudia-anthropic-api-version "2023-06-01"
  "Anthropic API version."
  :type 'string
  :group 'claudia)

(defcustom claudia-anthropic-api-max-tokens 1024
  "The maximum number of tokens to generate before stopping."
  :type 'integer
  :group 'claudia)

(defcustom claudia-default-project-name "created with claudia.el"
  "If non-nil use this name for new projects."
  :type 'string
  :group 'claudia)

(defcustom claudia-default-project-description "created with claudia.el"
  "If non-nil use this name for new projects."
  :type 'string
  :group 'claudia)

(defcustom claudia-default-project-prompt-template nil
  "If non-nil use this prompt template for new projects."
  :type 'string
  :group 'claudia)

(defcustom claudia-default-chat-name "[claudia.el]"
  "If non-nil use this name for new chats."
  :type 'string
  :group 'claudia)

(defcustom claudia-max-recent-buffers nil
  "Max number of recent buffers added to the project's knowledge."
  :type 'string
  :group 'claudia)

(defcustom claudia-max-recent-buffers-content-length 100000
  "Max number of recent buffers added to the project's knowledge."
  :type 'integer
  :group 'claudia)

(defcustom claudia-ignore-buffers-regexps nil
  "List of regexps for matching buffer names not added to the project's knowledge."
  :type '(repeat regexp)
  :group 'claudia)

(defcustom claudia-ignore-buffers-major-mode-regexps nil
  "List of regexps for matching buffer modes not added to the project's knowledge."
  :type '(repeat regexp)
  :group 'claudia)

(defvar claudia--current-project nil
  "ID of current project on Claude.ai.")

(defvar claudia--current-chat nil
  "ID of current chat conversation on Claude.ai.")

(defvar claudia--current-buffer nil
  "Buffer used for tool use.")

(defcustom claudia-instruction-initial
  "For this entire conversation, please skip any introductory
   paragraphs or context restatements in your responses. Be concise
   and focus on the technical details. Never repeat identical code
   from previous responses. Don't apologize for any confusion."
  "Set the initial instruction for Claude AI interactions."
  :type 'string
  :group 'claudia)

(defcustom claudia-instruction-confirmation
  "Please confirm you understand and will follow these instructions."
  "Set last instruction (confirmation) to Claude before starting a new chat."
  :type 'string
  :group 'claudia)

(defcustom claudia-instruction-markdown
  "For this entire conversation, please format all your responses in
   valid Markdown. This includes:

- Using proper heading levels (# for main headings, ## for
- subheadings, etc.)  Correctly formatting lists (both ordered and
- unordered) Using backticks for inline code and triple backticks for
- code blocks Properly formatting links, bold, and italic text
- Separating paragraphs with blank lines Using blockquotes where
- appropriate"
  "Instruct Cladia to respond with valid markdown."
  :type 'string
  :group 'claudia)

(defcustom claudia-instruction-explain-region
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
  "The instructions given to Claude when running `claudia-explain-region'."
  :type 'string
  :group 'claudia)

(defcustom claudia-magit-commit-msg-instruction
  "Please suggest a concise and informative commit message based on the diff I
just put in your knowledge context. The message should follow this format:

<verb> <subject> <description>

[optional body]

The commit verb can be one like `add',`remove',`fix',`include' etc.

The optional commit body should be used to provide further detail that
cannot fit within the character limitations of the subject line
description.

Provide the RAW COMMIT MESSAGE WITHOUT ANY ADDITIONAL TEXT.  Think
really hard about the `magit-diff' in your knowledge context."
  "The instructions given to Claude when running `claudia-suggest-commit-msg'."
  :type 'string
  :group 'claudia)

(defcustom claudia-instruction-summarize-pr
  "Please provide a concise summary of the changes in this pull request. Include:
1. The title of the PR
1. The main purpose or goal of the changes
2. Key files or components modified
3. Any notable additions, deletions, or modifications
4. Potential impact or implications of these changes

Please be brief but informative, focusing on the technical details and
the most important aspects of the diff."
  "The prompt for `claudia-summarize-gh-pr-from-url'."
  :type 'string
  :group 'claudia)

(defcustom claudia-instruction-summarize-web-page
  "Please provide a concise, but rather deep, summary of the
  web page content from %s. Include the main topics, key points, and
  any significant information."
  "The prompt for `claudia-summarize-page-from-url' for summarizing URL."
  :type 'string
  :group 'claudia)

(defcustom claudia-instruction-explain-symbol
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

;; claude.ai api

(defcustom claudia-api-url "https://api.claude.ai/api"
  "Base URL for claude.ai API."
  :type 'string
  :group 'claudia)

(defcustom claudia-session-key nil
  "Claude session key as a string."
  :type 'string
  :group 'claudia)

(defcustom claudia-organization-id nil
  "Claude organization ID as a string."
  :type 'string
  :group 'claudia)

(defcustom claudia-model "claude-3-5-sonnet-20240620"
  "The model to use for new chats.  Possible values are:

- claude-3-5-sonnet-20450620
- claude-3-opus-20240229
- claude-3-haiku-2024030"
  :type 'string
  :group 'claudia)

(defun claudia--claude-ai-request-assert-status (expected type endpoint)
  "Assert that latest request of TYPE to ENDPOINT had EXPECTED http status."
  (point-min)
  (search-forward " ")
  (let ((status (substring-no-properties (thing-at-point 'word))))
    (unless (string= status expected)
      (error "Claude API error, status: %s (expected %s)\nendpoint: %s %s"
             status expected type endpoint))))

(defun claudia--claude-ai-request-strip-header ()
  "Strip HTTP headers from current buffer starting at point-min."
  (re-search-forward "^$")
  (forward-line)
  (delete-region (point-min) (point))
  (point-min))

(defun claudia--claude-ai-parse-sse ()
  "Parse event stream.
The stream is parsed according to
https://html.spec.whatwg.org/multipage/server-sent-events.html
and https://docs.anthropic.com/en/api/messages-streaming

Note: this doesn't handle all kinds of event types, but it should suffice for
basic completion event streams."
  (point-min)
  (let ((event-type) (data) (text ""))
    (while (condition-case nil
               (search-forward "event: ")
             (search-failed nil))
      (setq event-type (buffer-substring-no-properties
                        (point) (line-end-position)))
      (search-forward "data: ")
      (setq data (json-read-from-string
                  (buffer-substring-no-properties
                   (point) (line-end-position))))
      (pcase event-type
        ("content_block_delta"
         (setq text (concat text (alist-get 'text (alist-get 'delta data)))))))
    ;; strip whitespace at the beginning of responses
    (unless (string= text "")
      (setq text (substring text 1)))
    text))

(defvar claudia--url-retrieve-silent t)
(defvar claudia--url-retrieve-inhibit-cookies nil)

(cl-defun claudia--claude-ai-request
    (endpoint &key
              (type "GET")
              (expect-status "200")
              (data nil)
              (content-type 'application/json)
              (callback-success nil)
              (encoding 'ascii))
  "Make request to Claude API at ENDPOINT with optional request parameters.
Send request of type TYPE to ENDPOINT expecting HTTP status EXPECT-STATUS. The
request body DATA if non-nil is encoded using ENCODING and sent with
CONTENT-TYPE. On success, CALLBACK-SUCCESS is called with the response buffer if
provided."
  (let* ((url (format "%s/organizations/%s/%s"
                      claudia-api-url claudia-organization-id endpoint))
         (headers
          `(("Content-Type" . ,(symbol-name content-type))
            ("Cookie" . ,(format "sessionKey=%s" claudia-session-key))))
         (url-request-extra-headers headers)
         (url-request-method type)
         (url-retrieve-number-of-calls 1)
         (url-request-data (and data (encode-coding-string data encoding))))
    (url-retrieve
     url
     (lambda (_status expect-status callback-success)
       (claudia--claude-ai-request-assert-status expect-status type endpoint)
       (claudia--claude-ai-request-strip-header)
       (if callback-success
           (funcall callback-success (current-buffer)))
       (url-mark-buffer-as-dead (current-buffer)))
     (list expect-status callback-success)
     claudia--url-retrieve-silent
     claudia--url-retrieve-inhibit-cookies)))

(defun claudia--claude-ai-json-callback (callback)
  "Wrap CALLBACK to parse its input as json before being called."
  (lambda (buf)
    (let ((content (with-current-buffer buf (json-read))))
      (funcall callback content))))

(defun claudia--claude-ai-simple-json-callback (msg property)
  "A simple json callback, printing out PROPERTY of its input along with MSG."
  (claudia--claude-ai-json-callback
   (lambda (response)
     (let ((val (alist-get property response)))
       (message "%s: %s" msg val)))))

(defun claudia--claude-ai-sse-callback (callback)
  "Wrap CALLBACK to parse its input as server-side-event before being called."
  (lambda (buf)
    (funcall
     callback
     (with-current-buffer
         buf (claudia--claude-ai-parse-sse)))))


;; projects

(defun claudia--claude-ai-request-get-projects (callback)
  "Call CALLBACK with the result of requesting all projects from Claude.ai."
  (let ((callback (claudia--claude-ai-json-callback callback)))
    (claudia--claude-ai-request
     "projects"
     :type "GET"
     :callback-success callback)))

(defun claudia--claude-ai-request-post-project (&optional name desc template callback)
  "Create a new Claude.ai project.
Optionally create it with NAME, description DESC, prompting TEMPLATE.  If
CALLBACK is non-nil it is called with the resulting json object from creating
the project."
  (unless name (setq name "[no name]"))
  (unless desc (setq desc "[no desc]"))
  (let* ((payload `(("name" . ,name)
                    ("description" . ,desc)
                    ("is_private" . t)
                    ("prompt_template" . ,template)))
         (callback (or callback
                       (claudia--claude-ai-simple-json-callback
                        "project created"
                        'uuid))))
    (claudia--claude-ai-request
     "projects"
     :type "POST"
     :expect-status "201"
     :data (json-encode payload)
     :callback-success callback)))

(defun claudia--claude-ai-request-delete-project (id)
  "Delete project with ID from Claude.ai and message when deletion is complete."
  (claudia--claude-ai-request
   (format "projects/%s" id)
   :type "DELETE"
   :expect-status "204"
   :callback-success (lambda (_) (message "project deleted"))))

(defun claudia--claude-ai-request-put-project (project prompt-template)
  "Update project PROJECT with PROMPT-TEMPLATE using Claude.ai PUT endpoint."
  (let ((payload `(("prompt_template" . ,prompt-template)))
        (callback (claudia--claude-ai-simple-json-callback "updated project" 'uuid)))
    (claudia--claude-ai-request
     (format "projects/%s" project)
     :type "PUT"
     :data (json-encode payload)
     :expect-status "202"
     :callback-success callback)))

;; project docs

(defun claudia--claude-ai-request-post-project-docs (project file-name content &optional callback)
  "Add document to PROJECT with FILE-NAME and CONTENT, then maybe call CALLBACK.
When CALLBACK is nil, a simple message is displayed when the document is
created."
  (let ((payload `(("file_name" . ,file-name) ("content" . ,content)))
        (callback (if callback
                      (claudia--claude-ai-json-callback callback)
                    (claudia--claude-ai-simple-json-callback
                     "project doc created "
                     'file_name))))
    (claudia--claude-ai-request
     (format "projects/%s/docs" project)
     :type "POST"
     :expect-status "201"
     :data (json-encode payload)
     :callback-success callback)))

(defun claudia--claude-ai-request-get-project-docs (project callback)
  "Get documents from PROJECT and call CALLBACK with json-encoded response."
  (let ((callback (claudia--claude-ai-json-callback callback)))
    (claudia--claude-ai-request
     (format "projects/%s/docs" project)
     :type "GET"
     :expect-status "200"
     :callback-success callback)))

(defun claudia--claude-ai-request-delete-project-doc (project doc &optional silent)
  "Delete document DOC from PROJECT, message unless SILENT is non-nil."
  (claudia--claude-ai-request
   (format "projects/%s/docs/%s" project doc)
   :type "DELETE"
   :expect-status "204"
   :callback-success (unless silent (lambda (_) (message "project doc deleted")))))

;; chats

(defun claudia--claude-ai-request-post-chat (name &optional project model callback)
  "Create chat with NAME, maybe bound to PROJECT and MODEL and call CALLBACK."
  (let ((payload `(("uuid" . ,(format "%s" (uuidgen-4)))
                   ("name" . ,name)
                   ("model" . ,model)
                   ("include_conversation_preferences" . t)))
        (callback (or callback
                      (claudia--claude-ai-simple-json-callback "chat created" 'uuid))))
    (if project (setq payload (cons `("project_uuid" . ,project) payload)))
    (claudia--claude-ai-request
     "chat_conversations"
     :type "POST"
     :expect-status "201"
     :data (json-encode payload)
     :callback-success callback)))

(defun claudia--claude-ai-request-get-chats (callback)
  "Get available chats from Claude.ai and call CALLBACK with json response."
  (let ((callback (claudia--claude-ai-json-callback callback)))
    (claudia--claude-ai-request
     "chat_conversations"
     :type "GET"
     :callback-success callback)))

(defun claudia--claude-ai-request-get-chat (id callback)
  "Get chat with ID from Claude.ai and call CALLBACK with json response tree."
  (let ((callback (claudia--claude-ai-json-callback callback)))
    (claudia--claude-ai-request
     (format "chat_conversations/%s?tree=true&rendering_mode=messages" id)
     :type "GET"
     :callback-success callback)))

(defun claudia--claude-ai-request-delete-chat (id)
  "Delete chat with ID from Claude.ai and message when deletion is complete."
  (claudia--claude-ai-request
   (format "chat_conversations/%s" id)
   :type "DELETE"
   :expect-status "204"
   :callback-success (lambda (_) (message "chat deleted"))))

;; chat completion

(defun claudia--claude-ai-request-post-chat-completion (chat prompt callback)
  "Send PROMPT to chat CHAT and call CALLBACK with server-sent event response."
  (let ((payload `(("prompt" . ,prompt) ("rendering_mode" . "messages")))
        (callback (claudia--claude-ai-sse-callback callback)))
    (claudia--claude-ai-request
     (format "chat_conversations/%s/completion" chat)
     :type "POST"
     :data (json-encode payload)
     :callback-success callback)))

;; chat mode

(defvar claudia--chat-prompt-regex "^\\*\\*\\(You\\)\\|\\(Claude\\)\\*\\*: ")
(defvar claudia--chat-prompt-regex-you "^\\(?1:\\*\\*You\\*\\*: (.*)\n\\(?2:.*\\)\\)")

(defun claudia-chat-next-prompt ()
  "Move to the next prompt in the Claude chat buffer."
  (interactive)
  (claudia--assert-chat-buffer-is-showing)
  (forward-line)
  (if (search-forward-regexp claudia--chat-prompt-regex nil t)
      (beginning-of-line)
    (user-error "No more prompts found")))

(defun claudia-chat-previous-prompt ()
  "Move to the previous prompt in the Claude chat buffer."
  (interactive)
  (claudia--assert-chat-buffer-is-showing)
  (if (search-backward-regexp claudia--chat-prompt-regex-you nil t)
      (beginning-of-line)
    (user-error "No previous prompts found")))

(defun claudia--imenu-create-chat-index ()
  "Create and return a flat imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward claudia--chat-prompt-regex-you (point-max) t)
        (let ((pos (match-beginning 1))
              (heading (match-string-no-properties 2)))
          (setq index (append index (list (cons heading pos))))))
      index)))

(defvar claudia-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'claudia-prompt)
    (define-key map (kbd "n") 'claudia-chat-next-prompt)
    (define-key map (kbd "p") 'claudia-chat-previous-prompt)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `claudia-chat-mode'.")

;;;###autoload
(define-derived-mode claudia-chat-mode markdown-mode "claudia:chat"
  "Major mode used for displaying Claude chat sessions."
  (read-only-mode)
  (setq imenu-create-index-function
        #'claudia--imenu-create-chat-index))

(defun claudia--chat-buffer ()
  "Get or create the current chat list buffer."
  (with-current-buffer (get-buffer-create "*claudia-chat*")
    (claudia-chat-mode)
    (current-buffer)))

(defun claudia--assert-chat-buffer-is-showing ()
  "Signal a user-error if the chat buffer is not the current buffer."
  (let ((buf (claudia--chat-buffer)))
    (unless (eq buf (current-buffer))
      (user-error "%s not the current buffer" (buffer-name buf)))))

(defun claudia--chat-erase-buffer ()
  "Erase the content of the *claudia-chat* buffer."
  (with-current-buffer (claudia--chat-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun claudia--refresh-chat-buffer ()
  "Refresh the buffer with the conversation from `claudia--current-chat'."
  (claudia--assert-current-chat-is-set)
  (claudia--claude-ai-request-get-chat
   claudia--current-chat
   #'claudia--refresh-chat-buffer-callback))

(defun claudia--refresh-chat-buffer-callback (chats)
  "Process CHATS message tree and render each message into current chat buffer.
Extracts sender, timestamp, and message content from each message, formats
appropriately for display, and inserts into buffer using
`claudia--chat-insert-entry'."
  (claudia--chat-erase-buffer)
  (let ((messages (alist-get 'chat_messages chats)))
    (dolist (msg (append messages nil))
      (let* ((sender (alist-get 'sender msg))
             (sender (if (string= sender "human") "You" "Claude"))
             (time (date-to-time (alist-get 'created_at msg)))
             ;; extract text string from content array
             (entry (cl-reduce
                     #'concat
                     (seq-map
                      (lambda (text-content)
                        (alist-get 'text text-content))
                      (seq-filter
                       (lambda (content)
                         (string= (alist-get 'type content) "text"))
                       (alist-get 'content msg)))))
             ;; trim first whitespace from Claude's responses
             (entry (if (string= sender "Claude")
                        (substring entry 1)
                      entry)))
        (claudia--chat-insert-entry sender entry time)))))

;; chat lists

(defvar claudia-chat-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'claudia-chat-list-select-chat)
    (define-key map (kbd "d") 'claudia-chat-list-mark-delete)
    (define-key map (kbd "u") 'claudia-chat-list-unmark)
    (define-key map (kbd "x") 'claudia-chat-list-execute-marks)
    (define-key map (kbd "r") 'claudia-chat-list-refresh)
    (define-key map (kbd "c") 'claudia-create-chat)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "s") 'claudia-chat-list-toggle-sorting-mode)
    map)
  "Keymap for `claudia-chat-list-mode'.")

(define-derived-mode claudia-chat-list-mode tabulated-list-mode "claudia:chats"
  "Major mode for listing Claude chat conversations."
  (let* ((window-width (- (window-width) 6))
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

;;;###autoload
(defun claudia-list-chats ()
  "Display a list of Claude chat conversations."
  (interactive)
  (let ((buffer (get-buffer-create "*claudia-chat-list*")))
    (with-current-buffer buffer
      (claudia-chat-list-mode)
      (claudia-chat-list-refresh))))

(defun claudia--safe-fmt-time (time)
  "Format TIME as date-time string, returning '[no-time]' if TIME is invalid."
  (condition-case nil
      (format-time-string "%F %T" (date-to-time time))
    (error "[no-time]")))

(defun claudia--chat-list-buffer ()
  "Get or create the current chat list buffer."
  (get-buffer-create "*claudia-chat-list*"))

;;;###autoload
(defun claudia-chat-list-refresh ()
  "Refresh the list of chat conversations."
  (interactive)
  (let ((callback
         (lambda (chats)
           (let ((list-entries
                  (cl-loop for chat across chats if chat collect
                           (let* ((project (alist-get 'project chat))
                                  (project-id-and-name
                                   (if project
                                       (cons (alist-get 'uuid project) (alist-get 'name project))
                                     (cons "[no project]" "[no project]")))
                                  (project-name
                                   (propertize (cdr project-id-and-name) 'project-id (car project-id-and-name))))
                             `(,(alist-get 'uuid chat)
                               [,(alist-get 'name chat)
                                ,project-name
                                ,(claudia--safe-fmt-time (alist-get 'updated_at chat))
                                ,(claudia--safe-fmt-time (alist-get 'created_at chat))
                                ])))))
             (with-current-buffer (claudia--chat-list-buffer)
               (setq tabulated-list-entries list-entries)
               (tabulated-list-print t)
               (switch-to-buffer (current-buffer)))
             (message "refreshed chat list")))))
    (claudia--claude-ai-request-get-chats callback)))


(defun claudia--assert-chat-list-is-showing ()
  "Signal a user-error if the chat list is not the current buffer."
  (let ((buf (claudia--chat-list-buffer)))
    (unless (eq buf (current-buffer))
      (user-error "%s not the current buffer" (buffer-name buf)))))

;;;###autoload
(defun claudia-chat-list-select-chat ()
  "Select the chat at point in the chat list."
  (interactive)
  (claudia--assert-chat-list-is-showing)
  (let* ((chat (tabulated-list-get-entry))
         (chat-id (tabulated-list-get-id))
         (project-name (elt chat 1))
         (project-id (get-text-property 0 'project-id project-name)))
    (setq claudia--current-project project-id)
    (setq claudia--current-chat chat-id)
    (claudia--refresh-chat-buffer)))

(defun claudia-chat-list-mark-delete ()
  "Mark a chat for deletion."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun claudia-chat-list-unmark ()
  "Unmark a chat."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun claudia-chat-list-execute-marks ()
  "Execute the marked actions (delete marked chats)."
  (interactive)
  (let (chats-to-delete)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((tag (char-after)))
          (when (eq tag ?D)
            (push (tabulated-list-get-id) chats-to-delete)))
        (forward-line 1)))
    (when chats-to-delete
      (if (yes-or-no-p (format "Delete %d marked chat(s)?" (length chats-to-delete)))
          (progn
            (dolist (chat-id chats-to-delete)
              (claudia--claude-ai-request-delete-chat chat-id))
            (claudia-chat-list-refresh))
        (message "Deletion cancelled")))))

;; basic functions to create/select projects, chats, docs etc.

(defun claudia--assert-current-project-is-set ()
  "Assert `claudia-current-project' is set."
  (unless claudia--current-project
    (user-error "No current project set.  Run `claudia-select-or-create-project' to select an existing one or create a new")))

(defun claudia--assert-current-chat-is-set ()
  "Assert `claudia-current-chat' is set."
  (unless claudia--current-chat
    (user-error "No current chat set.  Run `claudia-select-or-create-chat' to select an existing one or create a new")))

(defun claudia--assert-anthropic-api-key-is-set ()
  "Assert `claudia-anthropic-api-key' is set."
  (unless claudia-anthropic-api-key
    (user-error "No API key set. Customize `claudia-anthropic-api-key'")))

(defun claudia--claude-ai-new-project (name &optional description template)
  "Create a new Claude.ai project with NAME, DESCRIPTION and TEMPLATE."
  (let* ((desc (or description claudia-default-project-description
                   (read-string "project description: ")))
         (template (or template claudia-default-project-prompt-template "no template"))
         (callback
          (claudia--claude-ai-json-callback
           (lambda (response)
             (let ((id (alist-get 'uuid response)))
               (setq claudia--current-project id)
               (claudia--maybe-refresh-buffer-docs)
               (message "claude.ai project created"))))))
    (claudia--claude-ai-request-post-project name desc template callback)))

(defun claudia--claude-ai-new-chat (name prompt)
  "Create a new Claude.ai chat with NAME and starting PROMPT."
  (claudia--assert-current-project-is-set)
  (let* ((callback
          (claudia--claude-ai-json-callback
           (lambda (response)
             (let ((id (alist-get 'uuid response)))
               (message "Chat created. Waiting for Claude.ai completion...")
               (setq claudia--current-chat id)
               (claudia--chat-erase-buffer)
               (claudia--claude-ai-completion prompt))))))
    (claudia--claude-ai-request-post-chat
     name claudia--current-project claudia-model callback)))

(defvar claudia--uuid-regex "[[:xdigit:]]\\{8\\}-\\([[:xdigit:]]\\{4\\}-\\)\\{3\\}[[:xdigit:]]\\{12\\}")

;;;###autoload
(defun claudia-select-or-create-project ()
  "Set the current project with minibuffer completion."
  (interactive)
  (let ((callback (claudia--completing-read-callback
                   "Select project or enter a name to create a new one: "
                   (lambda (id-or-name)
                     (if (not (string-match-p claudia--uuid-regex id-or-name))
                         (claudia--claude-ai-new-project id-or-name)
                       (setq claudia--current-project id-or-name)
                       (claudia--maybe-refresh-buffer-docs)
                       (message "selected project: %s" id-or-name))
                     (setq claudia--current-chat nil)
                     (claudia--chat-erase-buffer)
                     (with-current-buffer (claudia--chat-buffer)
                       (let ((inhibit-read-only t))
                         (insert "no chat selected"))))
                   :require-match 'confirm)))
    (claudia--claude-ai-request-get-projects callback)))

;;;###autoload
(defun claudia-create-chat ()
  "Create a new chat in the current project.
Prompts for a name or uses `claudia-default-chat-name' if it is non-nil."
  (interactive)
  (claudia--assert-current-project-is-set)
  (let ((name (or claudia-default-chat-name
                  (read-string "Enter a name to create a new chat: ")))
        (prompt (read-string "prompt: ")))
    (claudia--claude-ai-new-chat name prompt)))

;;;###autoload
(defun claudia-select-or-create-chat ()
  "Set the current chat with minibuffer completion."
  (interactive)
  (claudia--assert-current-project-is-set)
  (let ((callback (claudia--completing-read-callback
                   "Select chat or enter a name to create a new one: "
                   (lambda (id-or-name)
                     (if (not (string-match-p claudia--uuid-regex id-or-name))
                         (let ((prompt (read-string "prompt: ")))
                           (claudia--claude-ai-new-chat id-or-name prompt))
                       (setq claudia--current-chat id-or-name)
                       (claudia--refresh-chat-buffer)
                       (message "selected chat: %s" id-or-name)))
                   :predicate
                   (lambda (c)
                     (let ((project
                            (alist-get
                             'uuid
                             (alist-get
                              'project
                              (cdr c)))))
                       (string= project claudia--current-project)))
                   :require-match 'confirm)))
    (claudia--claude-ai-request-get-chats callback)))

;;;###autoload
(defun claudia-delete-project-knowledge ()
  "Completing-reads a project doc to delete from the current project."
  (interactive)
  (claudia--assert-current-project-is-set)
  (let ((callback (claudia--completing-read-callback
                   "Select document to delete: "
                   (lambda (doc-id)
                     (claudia--claude-ai-request-delete-project-doc
                      claudia--current-project doc-id))
                   :name 'file_name)))
    (claudia--claude-ai-request-get-project-docs
     claudia--current-project callback)))

(cl-defun claudia--completing-read-callback
    (prompt callback
            &key
            (name 'name)
            (predicate nil)
            (require-match 't))
  "Return a callback for completing-reading with PROMPT and CALLBACK.
See `completing-read' for the meaning of the optional arguments NAME,
PREDICATE and REQUIRE-MATCH."
  (lambda (collection)
    (let* ((map-id (lambda (x) (cons (alist-get 'uuid x) x)))
           (completion-table (seq-map map-id collection))
           (annotation-fun (lambda (s)
                             (if-let ((val (assoc s minibuffer-completion-table))
                                      (name (alist-get name (cdr val))))
                                 (format " %s" name))))
           (completion-extra-properties (list :annotation-function annotation-fun))
           (selection (completing-read prompt completion-table predicate require-match)))
      (apply callback (list selection)))))


;; chat completion, i.e prompting claude.ai

(defcustom claudia-chat-display-buffer t
  "Wether to display the *claudia-chat* buffer when receiving new completions."
  :type 'boolean
  :group 'claudia)

(defun claudia--claude-ai-completion (prompt &optional callback inhibit-prompt)
  "Sends PROMPT to Claude.ai for completion in current chat.
If CALLBACK is non-nil it's called as a function as `(apply callback res)' where
res is a singleton list containing the result from Claude.ai as a string.
Otherwise the result is displayed in the *claudia-chat* buffer.  If
INHIBIT-PROMPT is non-nil don't insert the users prompt into the *claudia-chat*
buffer."
  (claudia--assert-current-chat-is-set)
  (unless inhibit-prompt (claudia--chat-insert-user-prompt prompt))
  (let ((callback (or callback #'claudia--chat-completion-callback)))
    (claudia--claude-ai-request-post-chat-completion
     claudia--current-chat prompt callback)))

(defun claudia--chat-completion-callback (res)
  "Insert `claudia--claude-ai-completion's resulting RES in *claudia-chat*."
  (claudia--chat-insert-ai-response res))

;;;###autoload
(defun claudia-prompt ()
  "Send prompt to Claude in the current chat conversation."
  (interactive)
  (claudia--assert-current-chat-is-set)
  (claudia--claude-ai-completion (read-string "prompt: ")))

(defun claudia--chat-insert-entry (entity entry &optional time)
  "Insert new ENTRY from ENTITY at TIME into the *claudia-chat* buffer."
  (setq time (format-time-string "%F %T" (or time (current-time))))
  (with-current-buffer (claudia--chat-buffer)
    (let ((chat-window (get-buffer-window (current-buffer)))
          (inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "**%s**: (%s)\n%s\n\n" entity time entry))
      (set-window-point chat-window (point-max))
      (if claudia-chat-display-buffer
          (display-buffer (current-buffer))))))

(defun claudia--chat-insert-user-prompt (prompt &optional time)
  "Insert new PROMPT from the user at TIME into the *claudia-chat* buffer."
  (claudia--chat-insert-entry "You" prompt time))

(defun claudia--chat-insert-ai-response (response &optional time)
  "Insert new RESPONSE from the user at TIME into the *claudia-chat* buffer."
  (claudia--chat-insert-entry "Claude" response time))

(defun claudia--chat-insert-tool-use (tool-def args &optional time)
  "Insert TOOL-DEF use with ARGS at TIME into the *claudia-chat* buffer."
  (let* ((tool-name (claudia-tool-name tool-def))
         (tool-name-fmt (if args (format "**%s**%s" tool-name args)
                          (format "**%s**" tool-name)))
         (buffer claudia--current-buffer)
         (response (format "[tool use: %s in buffer _%s_]" tool-name-fmt args buffer)))
    (claudia--chat-insert-entry "Claude" response time)))

(defcustom claudia-inhibit-prompt-to-kill-ring-in-chat t
  "Whether `claudia-prompt-to-kill-ring' prompts aren't echoed in the chat buffer.
This variable only has an effect in the current *claudia-chat* buffer.  When
old chat conversations are loaded the variable is not honered."
  :type 'string
  :group 'claudia)

;;;###autoload
(defun claudia-prompt-to-kill-ring ()
  "Send a prompt to Claude.ai and receive the respones in the `kill-ring'."
  (interactive)
  (claudia--assert-current-chat-is-set)
  (let* ((prompt (read-string "prompt: "))
         (callback (lambda (res)
                     (kill-new res)
                     (message "claudia: kill ring updated")))
         (inhibit-prompt (or claudia-inhibit-prompt-to-kill-ring-in-chat 't)))
    (claudia--claude-ai-completion prompt callback inhibit-prompt)))

;; project knowledge

;;;###autoload
(defun claudia-add-buffer-to-project-knowledge (buffer)
  "Add the content of BUFFER to the current project's knowledge."
  (interactive "bchoose buffer: ")
  (claudia--assert-current-project-is-set)
  (let* ((buffer (get-buffer buffer))
         (file-name (buffer-name buffer))
         (content (with-current-buffer buffer
                    (buffer-string))))
    (claudia--claude-ai-request-post-project-docs
     claudia--current-project file-name content)))

;;;###autoload
(defun claudia-add-region-to-project-knowledge ()
  "Add the current active region to the current project's knowledge."
  (interactive)
  (unless (use-region-p)
    (user-error "Region inactive"))
  (claudia--assert-current-project-is-set)
  (let* ((file-name (format "%s-region-[%s-%s]"
                            (buffer-name (current-buffer))
                            (region-beginning)
                            (region-end)))
         (content (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
    (claudia--claude-ai-request-post-project-docs
     claudia--current-project file-name content)))


;; claudia-mode

(defvar claudia--recent-buffers-alist nil
  "Alist of (buffer . metadata) recently added to the project's knowledge.")

(defun claudia--expand-buffer-name (buffer)
  "Expand BUFFER name."
  (concat "claudia-buffer:"
          (or (buffer-file-name buffer)
              (concat default-directory (buffer-name buffer)))))

(defun claudia--window-change-hook (_)
  "Update the project knowlege base when the current window change."
  (let* ((buffer (current-buffer))
         (file-name (claudia--expand-buffer-name buffer))
         (content (with-current-buffer buffer
                    (buffer-string))))
    (if (not (claudia--ignore-buffer buffer file-name))
        (claudia--claude-ai-request-post-project-docs
         claudia--current-project file-name content
         #'claudia--add-doc-to-recent-buffers-callback))))

(defun claudia--ignore-buffer (buffer file-name &optional allow-dup)
  "Wether to add BUFFER with FILE-NAME to `claudia--recent-buffers-alist'.
If ALLOW-DUP is non-nil then `claudia--recent-buffers-alist' is not checked for
an existing association for FILE-NAME."
  (or (and (not allow-dup)
           (assoc file-name claudia--recent-buffers-alist))
      (cl-some (lambda (re) (string-match-p re (buffer-name buffer)))
               claudia-ignore-buffers-regexps)
      (let ((buf-mode (with-current-buffer buffer
                        (symbol-name major-mode))))
        (cl-some (lambda (re) (string-match-p re buf-mode))
                 claudia-ignore-buffers-major-mode-regexps))))

(defun claudia--docs-metadata (doc)
  "Extract metadata from DOC."
  (let* ((name (alist-get 'file_name doc))
         (id (alist-get 'uuid doc))
         (content (alist-get 'content doc))
         (size (length content)))
    `(,name . (id ,id size ,size))))

(defun claudia--maybe-cleanup-buffer-docs ()
  "Remove docs from `claudia--recent-buffers-alist' and the current project.
If `claudia-max-recent-buffers' is set, then no more `claudia-buffer' docs will
be kept in the project knowledge.  If
`claudia-max-recent-buffers-content-length' is set to a number, then docs will
be cleaned up once the total number of characeters in `claudia-buffer' buffers
exceeds its value (starting with the least recently visited buffer's doc)."
  (let ((delete-next-and
         (lambda (continuation)
           (let* ((next (pop claudia--recent-buffers-alist))
                  (doc-id (plist-get (cdr next) 'id)))
             (claudia--claude-ai-request-delete-project-doc
              claudia--current-project doc-id t)
             (and continuation (funcall continuation next))))))
    (if claudia-max-recent-buffers
        (while (length> claudia--recent-buffers-alist claudia-max-recent-buffers)
          (funcall delete-next-and nil)))
    (if claudia-max-recent-buffers-content-length
        (let ((content-length (cl-reduce (lambda (acc c) (+ acc (plist-get (cdr c) 'size)))
                                         claudia--recent-buffers-alist
                                         :initial-value 0)))
          (while (> content-length claudia-max-recent-buffers-content-length)
            (funcall
             delete-next-and
             (lambda (next)
               (setq content-length
                     (- content-length (plist-get (cdr next) 'size))))))))))

(defun claudia--maybe-refresh-buffer-docs ()
  "Set `claudia--recent-buffers-alist' from the current project's knowledge.
This function is called when switching to a different project, but only has an
effect if `claudia-mode' is active."
  (when claudia-mode
    (claudia--assert-current-project-is-set)
    ;; remove the window hook to avoid races
    (remove-hook 'window-selection-change-functions #'claudia--window-change-hook)
    (let ((callback (lambda (docs)
                      (setq claudia--recent-buffers-alist nil)
                      (dolist (doc (append docs nil))
                        (when-let* ((name (alist-get 'file_name doc))
                                    (is-buffer-doc (string-match-p "^claudia-buffer:.*" name))
                                    (data (claudia--docs-metadata doc)))
                          (add-to-list 'claudia--recent-buffers-alist data t)))
                      (add-hook
                       'window-selection-change-functions
                       #'claudia--window-change-hook))))
      (claudia--claude-ai-request-get-project-docs
       claudia--current-project callback))))

(defun claudia--after-save-hook ()
  "Hook to update project knowledge when a buffer is saved."
  (when claudia-mode
    (claudia--assert-current-project-is-set)
    (let* ((buffer (current-buffer))
           (file-name (claudia--expand-buffer-name buffer))
           (content (with-current-buffer buffer
                      (buffer-string))))
      (unless (claudia--ignore-buffer buffer file-name t)
        ;; delete existing project doc for buffer
        (when-let* ((entry (assoc file-name claudia--recent-buffers-alist))
                    (doc-id (plist-get (cdr entry) 'id)))
          (setq claudia--recent-buffers-alist
                (assoc-delete-all file-name claudia--recent-buffers-alist))
          (claudia--claude-ai-request-delete-project-doc
           claudia--current-project doc-id t))
        ;; create project doc again
        (claudia--claude-ai-request-post-project-docs
         claudia--current-project file-name content
         #'claudia--add-doc-to-recent-buffers-callback)))))

;;;###autoload
(define-minor-mode claudia-mode
  "Enhance Emacs with AI tools based on Claude.ai."
  :global t :group 'claudia
  :lighter " claudia"
  (if claudia-mode
      (progn
        (unless claudia-session-key
          (user-error "`claudia-session-key' not set"))
        (unless claudia-organization-id
          (user-error "`claudia-organization-id' not set"))
        (claudia--assert-current-project-is-set)
        (add-hook 'window-selection-change-functions #'claudia--window-change-hook)
        (add-hook 'after-save-hook #'claudia--after-save-hook)
        (claudia--maybe-refresh-buffer-docs))
    (remove-hook 'window-selection-change-functions #'claudia--window-change-hook)
    (remove-hook 'after-save-hook #'claudia--after-save-hook)
    (setq claudia--recent-buffers-alist nil)))

(defun claudia--add-doc-to-recent-buffers-callback (res)
  "Callback for adding the project doc from RES to the recent buffers alist."
  (when claudia-mode
    (let ((data (claudia--docs-metadata res)))
      (add-to-list 'claudia--recent-buffers-alist data t)
      (claudia--maybe-cleanup-buffer-docs))))

(provide 'claudia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anthropic.com api

(cl-defun claudia--anthropic-api-request
    (endpoint &key
              (type "POST")
              (expect-status "200")
              (data nil)
              (content-type 'application/json)
              (callback-success nil)
              (encoding 'ascii))
  "Make request to Anthropic API at ENDPOINT with optional request parameters.
Send request of type TYPE to ENDPOINT expecting HTTP status EXPECT-STATUS. The
request body DATA if non-nil is encoded using ENCODING and sent with
CONTENT-TYPE. On success, CALLBACK-SUCCESS is called with the response buffer if
provided."
  (let* ((url (concat claudia-anthropic-api-url endpoint))
         (headers
          `(("content-type"      . ,(symbol-name content-type))
            ("x-api-key"         . ,claudia-anthropic-api-key)
            ("anthropic-version" . ,claudia-anthropic-api-version)))
         (url-request-extra-headers headers)
         (url-request-method type)
         (url-retrieve-number-of-calls 1)
         (url-request-data (and data (encode-coding-string data encoding))))
    (url-retrieve
     url
     (lambda (_status expect-status callback-success)
       (claudia--claude-ai-request-assert-status expect-status type endpoint)
       (claudia--claude-ai-request-strip-header)
       (if callback-success
           (funcall callback-success (current-buffer))))
     (list expect-status callback-success)
     claudia--url-retrieve-silent
     claudia--url-retrieve-inhibit-cookies)))

;; tool use

;; -- tool definition

(cl-defstruct claudia-tool-param
  name
  type
  description)

(cl-defstruct claudia-tool-input-schema
  type
  properties
  required)

(cl-defstruct claudia-tool
  name
  description
  function
  ask-before-use
  input-schema)

;; -- tool application

(cl-defstruct claudia-tool-arg
  name
  value)

(cl-defstruct claudia-tool-app
  name
  description
  args)

(defun claudia--tool-to-alist (tool)
  "Converts TOOL to an alist for consumption by the anthropic API."
  `(("name" .
     ,(claudia-tool-name tool))
    ("description" .
     ,(claudia-tool-description tool))
    ("input_schema" .
     (("type" .
       ,(claudia-tool-input-schema-type
         (claudia-tool-input-schema tool)))
      ("properties" .
       ,(mapcar #'claudia--tool-param-to-alist
                (claudia-tool-input-schema-properties
                 (claudia-tool-input-schema tool))))
      ("required" .
       ,(claudia-tool-input-schema-required
         (claudia-tool-input-schema tool)))))))

(defun claudia--tool-param-to-alist (param)
  "Convert PARAM to an alist."
  (if (claudia-tool-param-p param)
      (let ((name (claudia-tool-param-name param))
            (type (claudia-tool-param-type param))
            (desc (claudia-tool-param-description param)))
        `(,name . (("type" . ,type)
                   ("desc" . ,desc))))))
;; tool use

(defcustom claudia-tools
  `(,(make-claudia-tool
      :name "get_buffer_content"
      :description "Returns the content of the current active buffer."
      :ask-before-use t
      :function (lambda () (buffer-string))
      :input-schema
      (make-claudia-tool-input-schema
       :type "object"
       :properties nil
       :required nil))
    ,(make-claudia-tool
      :name "get_function_definition"
      :description "Returns definition of the given function symbol."
      :ask-before-use t
      :function (lambda (symbol-name)
                  (let ((sym (intern-soft symbol-name)))
                    (when (fboundp sym)
                      (prin1-to-string (symbol-function sym)))))
      :input-schema
      (make-claudia-tool-input-schema
       :type "object"
       :properties
       `(,(make-claudia-tool-param
           :name "symbol"
           :type "string"
           :description "The function symbol (i.e name)"))
       :required '("symbol")))
    ,(make-claudia-tool
      :name "emacs_message"
      :description "Send a message to the *Messages* buffer"
      :ask-before-use t
      :function (lambda (msg) (message "%s" msg))
      :input-schema
      (make-claudia-tool-input-schema
       :type "object"
       :properties
       `(,(make-claudia-tool-param
           :name "msg"
           :type "string"
           :description "The message to send"))
       :required '("msg"))))
  "List of tools to use."
  :type '(list claudia-tool)
  :group 'claudia)

;; messages api

(defvar claudia--anthropic-current-chat-messages nil)

;;;###autoload
(defun claudia-anthropic-clear-messages ()
  "Reset chat message history."
  (interactive)
  (setq claudia--anthropic-current-chat-messages nil)
  (message "claudia: message history cleared"))

(defun claudia--append-user-message (message)
  "Append MESSAGE to `claudia--anthropic-current-chat-messages'."
  (setq claudia--anthropic-current-chat-messages
        (append
         claudia--anthropic-current-chat-messages
         `((("role" . "user")
            ("content" . ,message))))))

(defun claudia--filter-empty-tool-use-inputs (content)
  "Filter away empty tool use inputs from CONTENT.
AFAIK Anthropics messages API is designed so we should be able to echo back,
assistant messages directly in the conversation. However tool uses from nullary
tools seem to cause issues resulting in `tool input must be a dictionary'
errors."
  (let ((f (lambda (item)
             (if-let ((type (alist-get 'type item))
                      ((string= type "tool_use"))
                      ((not (alist-get 'input item))))
                 ;; filter away the item's nil input
                 ;; provide some dictionary value to avoid bad requests
                 `((type . ,type)
                   (id . ,(alist-get 'id item))
                   (name . ,(alist-get 'name item))
                   (input (FOO . 42)))
               item))))
    (vconcat (mapcar f content) nil)))

(defun claudia--append-assistant-message (message)
  "Append MESSAGE to `claudia--anthropic-current-chat-messages'."
  (let ((message (claudia--filter-empty-tool-use-inputs message)))
    (setq claudia--anthropic-current-chat-messages
          (append
           claudia--anthropic-current-chat-messages
           `((("role" . "assistant")
              ("content" . ,message)))))))

(defun claudia--anthropic-api-post-messages (callback &optional tools message)
  "Complete current messages with TOOLS and last MESSAGE, then call CALLBACK."
  (if message (claudia--append-user-message message))
  (let ((payload `(("model" . ,claudia-model)
                   ("max_tokens" . ,claudia-anthropic-api-max-tokens)
                   ("messages" . ,claudia--anthropic-current-chat-messages)
                   ("tools" . ,(mapcar #'claudia--tool-to-alist tools))))
        (callback (claudia--claude-ai-json-callback callback)))
    ;; (message "PAYLOAD: %s" payload)
    (claudia--anthropic-api-request
     "/messages"
     :data (json-encode payload)
     :callback-success callback)))

(defun claudia--get-tool-def-from-name (name)
  "Lookup tool def for NAME in `claudia-tools'."
  (cl-some (lambda (tool) (if (string= (claudia-tool-name tool) name) tool))
           claudia-tools))

(defun claudia--tool-use-from-response (content)
  "Helper for getting the tool use requests from the response CONTENT."
  (if-let* ((is-tool-use (string= (alist-get 'type content) "tool_use"))
            (name (alist-get 'name content))
            (tool-def (claudia--get-tool-def-from-name name)))
      (cons tool-def content)))

(defun claudia--execute-tool-prompt (tool-def)
  "Prompt for TOOL-DEF use with TOOL-ARGS."
  (format "Execute tool %s?" (claudia-tool-name tool-def)))

(defun claudia--maybe-ask-user-before-using-tool (tool-def)
  "Ask the user if TOOL-DEF should be executed."
  (if (claudia-tool-ask-before-use tool-def)
      (y-or-n-p (claudia--execute-tool-prompt tool-def)))
  t)

(defun claudia--execute-tool (tool-def tool-args)
  "Execute TOOL-DEF with TOOL-ARGS inside BUFFER."
  ;; (message "CLAUDIA--EXECUTE-TOOL")
  (let* ((fun (claudia-tool-function tool-def))
         (schema (claudia-tool-input-schema tool-def))
         (schema-props (claudia-tool-input-schema-properties schema))
         (args nil))
    ;; build argument list by matching formal params with actual args
    (dolist (param (reverse schema-props))
      (let* ((param-name (claudia-tool-param-name param))
             ;;(param-type (claudia-tool-param-type param))
             (arg-val (cl-some (lambda (arg)
                                 (let ((arg-name (car arg))
                                       (arg-val  (cdr arg)))
                                   (if (string= arg-name param-name)
                                       arg-val)))
                               tool-args)))
        (setq args (cons arg-val args))))
    (claudia--chat-insert-tool-use tool-def args)
    (with-current-buffer claudia--current-buffer
      ;; (message "EXECUTING TOOL %s: %s %s" (tool-name tool-def) fun args)
      (apply fun args))))

(defun claudia--convert-value-to-string (value)
  "Convert VALUE received from Claude to a string."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   (t (prin1-to-string value))))

(defun claudia--fmt-tool-response (response tool-use)
  "Format RESPONSE from TOOL-USE to send back to anthropic API."
  (let ((tool-id (alist-get 'id tool-use))
        (res (claudia--convert-value-to-string response)))
    `(("type" . "tool_result")
      ("tool_use_id" . ,tool-id)
      ("content" . ,res))))

(defun claudia--handle-tool-use (content)
  "Executes all tool uses from CONTENT and send back results."
  (let* ((tool-uses (seq-map
                     #'claudia--tool-use-from-response
                     (seq-filter #'claudia--tool-use-from-response content)))
         (tool-results nil))
    ;; execute each tool and accumulate the results
    (dolist (tool tool-uses)
      (when-let* ((tool-def (car tool))
                  (tool-use (cdr tool))
                  (should-use (claudia--maybe-ask-user-before-using-tool tool-def)))
        (let* ((tool-args (alist-get 'input tool-use))
               (res (claudia--execute-tool tool-def tool-args))
               (res-with-meta (claudia--fmt-tool-response res tool-use)))
          (setq tool-results (cons res-with-meta tool-results)))))
    ;; send back results
    (claudia--append-user-message (vconcat tool-results))
    (claudia--anthropic-api-post-messages
     #'claudia--anthropic-api-callback claudia-tools)))


(defun claudia--anthropic-api-callback (response)
  "Process stop reason in RESPONSE by calling dedicated functions."
  (let* ((stop-reason (alist-get 'stop_reason response))
         (content (alist-get 'content response))
         (text (cl-some
                (lambda (c) (if (string= (alist-get 'type c) "text")
                           (alist-get 'text c)))
                content)))
    (claudia--chat-insert-ai-response text)
    (claudia--append-assistant-message content)
    (pcase stop-reason
      ("tool_use" (claudia--handle-tool-use content))
      ("end_turn" (message "Claudia ended its turn"))
      ("max_tokens" (error "Received stop reason: `max_tokens'"))
      (_ (error "Unknown stop reason: %s" stop-reason)))))

;;;###autoload
(defun claudia-prompt-with-tool-use ()
  "Send to Anthropic API with tool use."
  (interactive)
  (claudia--assert-anthropic-api-key-is-set)
  (let ((prompt (read-string "prompt: "))
        (inhibit-prompt nil))
    (unless (string-empty-p prompt)
      (unless inhibit-prompt (claudia--chat-insert-user-prompt prompt))
      (setq claudia--current-buffer (current-buffer))
      (claudia--anthropic-api-post-messages
       #'claudia--anthropic-api-callback claudia-tools prompt))))

;;; claudia.el ends here
