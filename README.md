# Claudia

Claudia is an Emacs integration for the Claude AI assistant, providing an interface between Emacs and Anthropic's language models. Claudia aims to provide features that integrate well with built-in Emacs functions and commonly used 3rd party libraries, such as explaining the currently marked content (code region), explaining the purpose of the symbol at point, utilizing eglot to enhance the prompts with static code analysis, generating git commit messages and more.

> *Note*: claudia is currently in pre-alpha, so expect breaking changes and rough edges.

## Features

- Create and manage AI projects and conversations within Emacs
- Add buffer content and web pages to project knowledge.
- Explain currently marked code regions.
- Generate commit messages based on current git diff
- Summarize web pages and GitHub pull requests
- Customizable instructions for each action (via customize)

## Installation

#### Install from MELPA

To install claudia from MELPA with `use-package`:

```elisp
(use-package claudia
  :bind (("C-x c p" . claudia-create-project)
         ("C-x c c" . claudia-create-chat)
         ("C-x c k" . claudia-clear-context)
         ("C-x c q" . claudia-query)
         ("C-x c l" . claudia-list-chats)
         ("C-x c s" . claudia-send-visiting-buffer)
         ("C-x c w" . claudia-summarize-page-from-url)
         ("C-x c e r" . claudia-explain-region)
         ("C-x c e s" . claudia-eglot-explain-symbol-at-point)
         ("C-x c g m" . claudia-suggest-commit-msg)
         ("C-x c g p" . claudia-gh-summarize-pr-from-url))
  :config
  (claudia-api-key "your-api-key-here")
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

Customize `claudia-api-key` and `claudia-organization-id` with their values (as shown above, using `use-package`).

## Usage

### Basic Commands

The following commands are used to organize projects and chats:

- `claudia-create-project`: Create a new Claude project
- `claudia-create-chat`: Start a new chat in the current project
- `claudia-list-chats`: Display a list of your chat conversations in a dedicated buffer. This activates a dedicated `claudia:chats` major mode where the following keybindings are available:
  - `RET` (`claudia-chat-list-select-chat`): Select the chat at point
  - `d` (`claudia-chat-list-mark-delete`): Mark a chat for deletion
  - `u` (`claudia-chat-list-unmark`): Unmark a chat
  - `x` (`claudia-chat-list-execute`): Execute deletion of marked chats
  - `r` (`claudia-chat-list-refresh`): Refresh the chat list
  - `s` (`claudia-chat-list-toggle-sorting-mode`): Toggle sorting mode
  - `c` (`claudia-create-chat`): Create a new chat

To reply to Claude in the current chat use `claudia-query`. This will optionally create a project and/or chat and open a dedicated `claudia:chat` buffer, where the following key bindings are available:

- `RET` and `q` (`claudia-query`): Reply to Claude in the current chat
- `e`(`claudia-explain-region`): Explain the currently marked region (see the variable `claudia--explain-region-instruction`).
- `c`(`claudia-create-chat`): Create a new chat in the current project, reusing the `claudia:chat` buffer.
- `k`(`claudia-clear-context`): Delete all project knowledge for the current project.
- `n`(`claudia--chat-next-prompt`): Move to your/ Claude's next response.
- `p`(`claudia--chat-previous-prompt`): Move to your previous response to Claude.
- `l`(`claudia-list-chats`): Display the chat list buffer
- `q`(`quit-window`): bury the buffer

### Adding project knowledge

claudia can upload the content of the current buffer as a file to the project's knowledge, using the command `claudia-send-visiting-buffer`, similar to how one can upload files to the project on Claude's web interface. This is a powerful feature to enhance prompts without spending a ton of tokens (files added as project knowledge doesn't count toward ones token usage, from my understanding). I use this to add a file and then ask questions about it. 

To remove _all_ files for the current project use the command `claudia-clear-context`.

### Code generation/ explanation

A main goal of Claudia is to integrate well with the built-in programming facilities in Emacs, as well as common 3rd party libraries.

Currently, the functions `claudia-explain-region` and `claudia-eglot-explain-symbol-at-point` can be used to explain code regions/ symbols. The former sends the content of the current region to Claude with the customizable prompt `claudia--explain-region-instruction`, asking Claude to provide a detailed explanation of what the code does, as well as explain any notable patterns/ idioms it uses. The latter integrates with eglot to enhance the prompt `claudia-eglot-explain-prompt` with the definition and all call-sites of the current symbol (e.g. function name). The variable `claudia-eglot-explain-symbol-xref-context` can be customized to change whether call-site context is collected using the language server's symbol provider, or as a fixed number of lines around the call-site.

The function `claudia-suggest-commit-msg` will generate a commit message based on the current git diff, and is meant to be run right after `magit-commit` pops up a new diff buffer. It fails if multiple magit diff buffers are open.

### Summarizing information

claudia currently has two different functions for summarizing information: `claudia-gh-summarize-pr-from-url)` will summarize key changes in a pull request (requires `gh` is installed), and `claudia-summarize-page-from-url` will summarize a web page (requires `lynx` is installed).

## Customization

Claudia provides several customizable variables:

- `claudia-api-key`: Your Claude API key
- `claudia-organization-id`: Your Claude organization ID
- `claudia-model`: The Claude model to use (default: "claude-3-5-sonnet-20240620")
- `claudia-gh-program`: Path to GitHub CLI executable (for PR summaries)
- `claudia-download-url-program`: Program used to download web pages (default: "lynx")
- `claudia-explain-include-context`: Whether to include surrounding code context in explanations
- `claudia-explain-context-len`: Number of context lines to include in explanations

### Instructions

Claudia uses several types of instructions to guide its interactions with Claude. These can be customized by modifying the following functions:

- `claudia--set-initial-instruction`: Sets the initial instruction for each conversation. By default, it instructs Claude to skip introductions, be concise, and focus on technical details.

- `claudia--set-markdown-config`: Configures Claude to format responses in valid Markdown, including proper headings, lists, code blocks, and text formatting. The claudia-chat mode derives from markdown-mode, so this makes responses look tasty.

- `claudia--set-last-instruction`: Sets the final instruction when starting a new chat, typically asking Claude to confirm understanding of all instructions.

- `claudia--explain-region-instruction`: Generates the instruction for explaining a selected code region, including file and mode context.

- `claudia--magit-commit-msg-instruction`: Provides instructions for generating commit messages based on git diffs.

- `claudia--gh-summarize-pr-instruction`: Creates instructions for summarizing GitHub pull requests.

- `claudia--web-summary-instruction`: Generates instructions for summarizing web pages.

## License

This project is licensed under the GPL-3.0 License - see the [LICENSE](LICENSE) file for details.
