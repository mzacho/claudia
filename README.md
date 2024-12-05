# Claudia

Claudia is an Emacs integration for the Claude AI assistant, providing an interface between Emacs and Anthropic's language models. Claudia aims to provide features that integrate well with built-in Emacs functions and commonly used 3rd party libraries, such as

- Maintaining a knowledge-base of the recently visited buffers
- Explaining the currently marked content (code region)
- Explaining the purpose of the symbol at point
- Utilizing eglot to enhance the prompts with static code analysis
- Display AI responses with pretty markdown formatting

> *Note*: claudia is currently in pre-alpha, so expect breaking changes and rough edges.

> *DISCLAIMER*: Claudia is not affiliated with Anthropic. By using Claudia you agree to use it at your own risk, and acknowledge potential violation of Anthropic's Terms of Service, for which you assume full responsibility for any consequences. Understand that Anthropic does not support this tool and please review [Anthropic's Terms of Service](https://www.anthropic.com/legal/consumer-terms) before using Claudia.

## Installation

#### Install from MELPA

To install claudia from MELPA with `use-package`:

```elisp
(use-package claudia
  :commands (claudia-mode)
  :custom
  (claudia-session-key "your-session-key-here")
  (claudia-organization-id "your-organization-id-here"))
```

#### Install manually (to hack or contribute to claudia)

1. Clone this repository:
   ```
   git clone https://github.com/mzacho/claudia.git
   ```
2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/claudia")
   (require 'claudia)
   ```
   
   or if using `use-package`:
   
   ```elisp
   (use-package claudia   
      :load-path "/path/to/claudia")
   ```

## Configuration

Claudia needs access to your Claude session key and your organization id.

The session key can be found by visiting [claude.ai](wwww.claude.ai), opening your browser's dev tools, going to the "Applications" tab and inspecting the "sessionKey" cookie.

The organization id can be found by going to the "Networks" tab in the dev tools, open an existing project on claude.ai and inspecting the URL of any of the requests to "api.claude.ai". For instance the one to `api.claude.ai/api/organizations/<your-organization-id>/projects`.

Customize `claudia-session-key` and `claudia-organization-id` with their values (as shown above, using `use-package`).

## Usage

`claudia.el` provides `claudia-mode`, a global minor mode which maintains a knowledge-base of recently visited buffers using Claude.ai's project knowledge feature. When the mode is enabled, active buffer contents are automatically added to project knowledge, enhancing the output of `claudia-prompt`. File saves updates the project knowledge, and content is managed within configured size limits. 

To enable the mode, `claudia--current-project` must first be set with `claudia-select-or-create-project`. Alternatively, a default "claudia" project can be set when loading the package (e.g. with `(setq claudia--current-project "your-project-uuid")`).

`claudia-mode` can be configured to ignore certain buffers based on their name or major mode, using the variables `claudia-ignore-buffers-regexps` and `claudia-ignore-buffers-major-mode-regexps`. The mode maintains a list of the buffers that are part of the knowledge base in the variable `claudia--recent-buffers-alist`. The list is occasionally pruned based on the value of the variables `claudia-max-recent-buffers` and `claudia-max-recent-buffers-content-length`.

### Basic Commands

The following commands are used to organize projects and chats:

- `claudia-prompt`: Send a prompt to Claude
- `claudia-select-or-create-project`: Select or create a project
- `claudia-select-or-create-chat`: Select or create a chat conversation
- `claudia-list-chats`: List all chats
- `claudia-delete-project-knowledge`: Delete project documents

<!-- ### Adding project knowledge -->

<!-- claudia can upload the content of the current buffer as a file to the project's knowledge, using the command `claudia-send-visiting-buffer`, similar to how one can upload files to the project on Claude's web interface. This is a powerful feature to enhance prompts without spending a ton of tokens (files added as project knowledge doesn't count toward ones token usage, from my understanding). I use this to add a file and then ask questions about it.  -->

<!-- To remove _all_ files for the current project use the command `claudia-clear-context`. -->

<!-- ### Code generation/ explanation -->

<!-- A main goal of Claudia is to integrate well with the built-in programming facilities in Emacs, as well as common 3rd party libraries. -->

<!-- Currently, the functions `claudia-explain-region` and `claudia-eglot-explain-symbol-at-point` can be used to explain code regions/ symbols. The former sends the content of the current region to Claude with the customizable prompt `claudia--explain-region-instruction`, asking Claude to provide a detailed explanation of what the code does, as well as explain any notable patterns/ idioms it uses. The latter integrates with eglot to enhance the prompt `claudia-eglot-explain-prompt` with the definition and all call-sites of the current symbol (e.g. function name). The variable `claudia-eglot-explain-symbol-xref-context` can be customized to change whether call-site context is collected using the language server's symbol provider, or as a fixed number of lines around the call-site. -->

<!-- The function `claudia-suggest-commit-msg` will generate a commit message based on the current git diff, and is meant to be run right after `magit-commit` pops up a new diff buffer. It fails if multiple magit diff buffers are open. -->

<!-- ### Summarizing information -->

<!-- claudia currently has two different functions for summarizing information: `claudia-gh-summarize-pr-from-url)` will summarize key changes in a pull request (requires `gh` is installed), and `claudia-summarize-page-from-url` will summarize a web page (requires `lynx` is installed). -->

## Customization

Claudia provides several customizable variables:

### Required Settings
- `claudia-session-key`: Your Claude.ai session key
- `claudia-organization-id`: Your Claude.ai organization ID

### Optional Settings
- `claudia-model`: Claude model to use (default: "claude-3-5-sonnet-20240620")
- `claudia-chat-display-buffer`: Whether to display chat buffer on response (default: t)
- `claudia-max-recent-buffers`: Maximum number of recent buffers to track (default: 3)
- `claudia-max-recent-buffers-content-length`: Maximum total content length for tracked buffers
- `claudia-ignore-buffers-regexps`: List of buffer name patterns to ignore
- `claudia-ignore-buffers-major-mode-regexps`: List of major modes to ignore
- `claudia-default-project-prompt-template`: New projects are created with this template

The author's configuration looks something like this:

```elisp
(use-package claudia
  :diminish claudia-mode
  :bind (("C-x c RET" . claudia-prompt)         
         ("C-x c c" . claudia-select-or-create-chat)
         ("C-x c p" . claudia-select-or-create-project)
         ("C-x c d" . claudia-delete-project-knowledge))         
  :custom  
  ;; Claude.ai session key and organization
  (claudia-session-key "your-session-key-here")
  (claudia-organization-id "your-organization-id-here")
  ;; My default project and chat
  (claudia--current-project "7dfeb125-3269-431e-b0c4-d11d85936732")
  (claudia--current-chat "deed76c0-3d24-4055-ab0a-c639868c2afb")

  ;; The *claudia-chat* window is always visible on my second monitor
  (claudia-chat-display-buffer nil)
  ;; Ignore most buffers not containing code
  (claudia-ignore-buffers-regexps '("\\*.*\\*" "magit.*"))
  (claudia-ignore-buffers-major-mode-regexps '("dired-mode" "pdf-view-mode"))
  ;; I find that 100000 characters of code is about 40% of the allowed project
  ;; knowledge on Claude.ai
  (claudia-max-recent-buffers nil)
  (claudia-max-recent-buffers-content-length 100000)
  ;; Instruct Claude.ai to deliver concise responses formatted in markdown
  (claudia-default-project-prompt-template
      (string-join
       (list claudia-instruction-initial
             claudia-instruction-markdown
             claudia-instruction-confirmation)
       " [NEXT INSTRUCTION] "))
    
  :config
  ;; Center cursor at the top of the *claudia-chat* window
  (add-hook 'claudia-chat-mode-hook
          (lambda ()
            (setq-local ccm-vpos-init 0)
            (centered-cursor-mode)))
  (claudia-mode))
```


## License

This project is licensed under the GPL-3.0 License - see the [LICENSE](LICENSE) file for details.
