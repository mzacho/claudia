# Claudia

Claudia is an Emacs integration for the Claude AI assistant, providing an interface between Emacs and Anthropic's language models.

## Features

- Create and manage AI projects and conversations within Emacs
- Add buffer content and web pages to project knowledge.
- Explain currently marked code regions.
- Generate commit messages based on current git diff
- Summarize web pages and GitHub pull requests
- Customizable instructions for each action (via customize)

## Installation

### Manual

Currently, Claudia is not available on MELPA. To install manually:

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

Add the following to your Emacs configuration:

```elisp
(setq claudia-api-key "your-api-key-here")
(setq claudia-organization-id "your-organization-id-here")
(global-set-key (kbd "C-x c") claudia-global-map)
```

Replace `"your-api-key-here"` and `"your-organization-id-here"` with your actual Claude API key and organization ID. The API key can be found using Chrome dev-tools under Cookies / sessionKey (it starts with `sk-ant-`) and the org ID can be read from the url in the Claude UI.

## Usage

### Basic Commands

All commands are accessible through the `claudia-global-map`, bound to `C-x c` by default:

- `C-x c p` (`claudia-create-project`): Create a new AI project
- `C-x c c` (`claudia-create-chat`): Start a new chat conversation
- `C-x c q` (`claudia-query`): Send a query to Claude
- `C-x c l` (`claudia-list-chats`): Display a list of your chat conversations
- `C-x c e` (`claudia-explain-region`): Ask Claude to explain the selected code region
- `C-x c w` (`claudia-summarize-page-from-url`): Summarize a web page
- `C-x c g m` (`claudia-suggest-commit-msg`): Generate a commit message based on the current git diff
- `C-x c g p` (`claudia-gh-summarize-pr-from-url)`: Summarize key changes in a pull request
- `C-x c k` (`claudia-clear-context`): Clear context (delete all project knowledge files)
- `C-x c s` (`claudia-send-visiting-buffer`): Send the content of the current buffer to Claude

### Chat List Buffer

In the chat list buffer (`claudia-chat-list-mode`):

- `RET` (`claudia-chat-list-select-chat`): Select the chat at point
- `d` (`claudia-chat-list-mark-delete`): Mark a chat for deletion
- `u` (`claudia-chat-list-unmark`): Unmark a chat
- `x` (`claudia-chat-list-execute`): Execute deletion of marked chats
- `r` (`claudia-chat-list-refresh`): Refresh the chat list
- `s` (`claudia-chat-list-toggle-sorting-mode`): Toggle sorting mode
- `c` (`claudia-create-chat`): Create a new chat

### Chat Buffer

In the chat buffer (`claudia-chat-mode`):

- `RET` (`claudia-query`): Send a query
- `n` (`claudia--chat-next-prompt`): Move to the next prompt
- `p` (`claudia--chat-previous-prompt`): Move to the previous prompt
- `c` (`claudia-create-chat`): Create a new chat
- `k` (`claudia-clear-context`): Clear context (delete all project knowledge files)
- `l` (`claudia-list-chats`): List chats
- `q` (`quit-window`): Quit the chat window


## Eglot integration

claudia integrates with eglot to enhance prompts with static code analysis using LSP. The following functions are available:

- `claudia-eglot-explain-symbol-at-point`: Lookup definition and references of the current symbol (function/ class/ variable etc.) and ask Claude to explain the purpose of the symbol as used in your code-base. The prompt can be customized with `claudia-eglot-explain-prompt` and `claudia-eglot-explain-symbol-xref-context`.

## Customization

Claudia provides several customizable variables:

- `claudia-api-key`: Your Claude API key
- `claudia-organization-id`: Your Claude organization ID
- `claudia-model`: The Claude model to use (default: "claude-3-5-sonnet-20240620")
- `claudia-gh-program`: Path to GitHub CLI executable (for PR summaries)
- `claudia-download-url-program`: Program used to download web pages (default: "lynx")
- `claudia-explain-include-context`: Whether to include surrounding code context in explanations
- `claudia-explain-context-len`: Number of context lines to include in explanations

Claudia uses several types of instructions to guide its interactions with Claude. These can be customized by modifying the following functions:

### Instructions

- `claudia--set-initial-instruction`: Sets the initial instruction for each conversation. By default, it instructs Claude to skip introductions, be concise, and focus on technical details.

- `claudia--set-markdown-config`: Configures Claude to format responses in valid Markdown, including proper headings, lists, code blocks, and text formatting. The claudia-chat mode derives from markdown-mode, so this makes responses look tasty.

- `claudia--set-last-instruction`: Sets the final instruction when starting a new chat, typically asking Claude to confirm understanding of all instructions.

- `claudia--explain-region-instruction`: Generates the instruction for explaining a selected code region, including file and mode context.

- `claudia--magit-commit-msg-instruction`: Provides instructions for generating commit messages based on git diffs.

- `claudia--gh-summarize-pr-instruction`: Creates instructions for summarizing GitHub pull requests.

- `claudia--web-summary-instruction`: Generates instructions for summarizing web pages.

## License

This project is licensed under the GPL-3.0 License - see the [LICENSE](LICENSE) file for details.
