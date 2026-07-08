# dot-emacs

A comprehensive Emacs 30.1 configuration focused on C++ development with AI-powered agentic coding and modern tooling.

## Overview

This configuration provides a feature-rich development environment optimized for C++ programming in a large monorepo, with deep AI integration via gptel and the gptel-agent package. The configuration is modular, organized into `elisp/` for standard config and `site-lisp/` for custom packages.

## Key Features

### AI-Powered Development
- **gptel**: LLM client connecting to an OpenAI-compatible backend with models like DeepSeek-V3.2, Qwen3-32B, GLM-5.2, Kimi-K2.6
- **gptel-agent**: Full agentic coding with tool use (file read/write, grep, glob, shell execution)
- **gptel-agent-loop**: Custom package to prevent agents from stopping prematurely — nudges the LLM to review task completion
- **gptel-cpp-complete**: AI-powered C/C++ completion using tree-sitter
- **Agent presets**: `gptel-qa` for general Q&A, `gptel-agent` for agentic tasks with tools
- **Context system**: `contexts/` directory with project rules automatically added to LLM context
- **Skills system**: Agent skills (à la Anthropic) loaded from `~/.emacs.d/skills`

### C++ Development
- **Eglot + clangd**: LSP with background indexing, clang-tidy, detailed completions
- **Tree-sitter**: c-ts-mode/c++-ts-mode for Emacs 29+
- **tempo-c-cpp**: Custom C/C++ template expansion (site-lisp)
- **Code navigation**: xref, consult-eglot-symbols, type/call hierarchy
- **Flymake + sideline-flymake**: Inline diagnostics display
- **Compilation**: Custom compile integration

### Completion & Navigation
- **Corfu**: In-buffer popup completion with auto-trigger
- **Cape**: Completion-at-point extensions (dabbrev, file, dict, keyword)
- **Vertico + Orderless + Marginalia**: Modern minibuffer completion stack
- **Consult**: Enhanced search, buffer switching, line jumping
- **Embark**: Contextual actions on completion candidates
- **Ivy/Counsel/Swiper**: Alternative completion framework (also installed)

### Version Control
- **Magit**: Full Git interface
- **diff-hl**: Inline gutter diff indicators
- **git-timemachine**: Browse file history
- **git-link**: Generate repository URLs
- **git-messenger**: Show commit message at point
- **dired-git-info**: Git info in dired buffers

### Productivity
- **Org-mode**: Extensive setup with org-modern, org-pomodoro, calfw calendar, org-cliplink
- **Multiple cursors**: Edit multiple locations simultaneously
- **expand-region**: Semantic text selection expansion
- **symbol-overlay**: Highlight and navigate symbols
- **Hydra / pretty-hydra**: Transient key menus
- **Popper**: Popup buffer management
- **writeroom-mode / olivetti**: Distraction-free writing
- **super-save**: Auto-save on focus change

### Search & Grep
- **rg.el**: Ripgrep integration
- **ag.el**: Silver searcher integration
- **consult-flycheck**: Diagnostics with consult
- **wgrep**: Editable grep buffers

### Language Support
- **C/C++**: Primary focus (eglot, tree-sitter, tempo templates)
- **Python**: Basic setup
- **JavaScript/TypeScript**: js2-mode, typescript-mode, prettier-js, xref-js2
- **Ruby**: Configuration present
- **YANG**: yang-mode for network data models
- **YAML/TOML/JSON**: Mode support
- **Lua**: lua-mode
- **Markdown**: markdown-mode
- **PlantUML**: plantuml-mode for diagrams
- **TTCN-3**: Custom mode in site-lisp for telecom test cases

### Terminal & Shell
- **eat**: Terminal emulator (Emulate A Terminal)
- **Shell integration**: Custom shell setup

### UI & Theme
- **Zenburn**: Color theme
- **nerd-icons**: Icons for dired, ibuffer, corfu, ivy-rich
- **mode-icons**: Mode line icons
- **indent-guide**: Visual indentation guides
- **hl-todo**: Highlight TODO/FIXME keywords
- **rainbow-mode**: Color code visualization

### Reading & Media
- **pdf-tools**: PDF viewer
- **nov.el**: EPUB reader
- **eww**: Enhanced web browsing with eww-lnum
- **bongo**: Music player
- **stock-tracker**: Stock market tracking

## Directory Structure

```
~/.emacs.d/
├── elisp/                   # Modular configuration (38 files)
│   ├── init-basics.el       # Fundamental settings, keybindings, locale
│   ├── init-elpa.el         # Package management
│   ├── init-3rd-party.el    # Third-party package configs
│   ├── init-cc.el           # C/C++ development
│   ├── init-eglot.el        # LSP (clangd)
│   ├── init-corfu.el        # In-buffer completion
│   ├── init-gptel.el        # LLM client configuration
│   ├── init-agent.el        # Agentic AI setup (gptel-agent)
│   ├── init-org.el          # Org-mode
│   ├── init-git.el          # Version control
│   ├── init-compile.el      # Build integration
│   ├── init-hydra.el        # Key menus
│   ├── init-ivy.el          # Ivy/Counsel completion
│   ├── init-minibuffer.el   # Vertico/Consult/Embark
│   ├── init-windows.el      # Window management
│   ├── init-productivity.el # Productivity tools
│   ├── init-flymake.el      # Diagnostics
│   ├── init-shell.el        # Shell configuration
│   ├── init-python.el       # Python
│   ├── init-javascript.el   # JavaScript/TypeScript
│   ├── init-ruby.el         # Ruby
│   ├── init-dired.el        # File manager
│   ├── init-eww.el          # Web browser
│   ├── init-calendar.el     # Calendar (calfw)
│   ├── init-session.el      # Session save/restore
│   ├── init-ui.el           # Visual customization
│   ├── init-isearch.el      # Search enhancements
│   ├── init-imenu.el        # Code outline
│   ├── init-grep.el         # Grep integration
│   ├── init-terminals.el    # Terminal emulator
│   ├── init-ibuffer.el      # Buffer list
│   ├── init-nxml.el         # XML editing
│   ├── init-css.el          # CSS
│   ├── init-utils.el        # Utility functions
│   ├── init-alias.el        # Command aliases
│   ├── init-whitespace.el   # Whitespace handling
│   └── init-benchmarking.el # Startup profiling
├── site-lisp/               # Custom/local packages
│   ├── gptel-agent-loop.el  # Agent loop resilience
│   ├── task-completion-rules.md
│   ├── tempo-c-cpp.el       # C/C++ templates
│   ├── ivy-preview.el       # Ivy file preview
│   ├── ipc-udp.el           # UDP IPC utilities
│   ├── ttcn3.el             # TTCN-3 major mode
│   └── esc-mode.el          # ESC key handling
├── agents/                  # LLM agent definitions
│   ├── gptel-telegram.md
│   ├── gptel-opencode-subagent.md
│   └── gptel-opencode-agent.md
├── contexts/                # LLM context files (auto-loaded)
│   └── general-rules.md
└── skills/                  # Agent skills (symlink)
```

## Key Bindings

### General
| Key | Command |
|-----|---------|
| `C-x C-m` | execute-extended-command (M-x alternative) |
| `C-w` | backward-kill-word-or-region |
| `C-x C-r` | recentf-open-files |
| `C-x u` | vundo (undo visualization) |
| `M-/` | completion-at-point |

### Code Navigation (Eglot)
| Key | Command |
|-----|---------|
| `M-.` | xref-find-definitions |
| `M-]` | xref-find-references |
| `M-?` | consult-eglot-symbols |
| `C-c d` | eglot-find-declaration |
| `C-c i` | eglot-find-implementation |
| `C-c r` | eglot-rename |
| `C-c h` | eglot-show-type-hierarchy |
| `C-c c` | eglot-show-call-hierarchy |
| `C-<return>` | eglot-code-action-quickfix |
| `<TAB>` | eglot-format |

### AI / LLM
| Key / Command | Description |
|---------------|-------------|
| `gptel` | Start an LLM chat session |
| `C-c RET` | Send buffer/region to LLM |
| `gptel-dwim` | Quick prompt with optional context |
| `gptel-rewrite` | Rewrite/refactor selection |
| `gptel-agent` | Launch agentic coding session |

### Git (Magit)
| Key | Command |
|-----|---------|
| `C-x g` | magit-status |

## LLM Backend Configuration

The config connects to an OpenAI-compatible endpoint via bearer token authentication. Configure your own backend by modifying `init-gptel.el`.

Available models (configurable):
- `deepseek-ai/DeepSeek-V3.2` (default, 128k context)
- `Qwen/Qwen3-32B` (32k context)
- `zai-org/GLM-5.2-FP8` (202k context)
- `moonshotai/Kimi-K2.6`
- `openai/gpt-oss-120b`

## Custom Packages

### gptel-agent-loop.el
Prevents gptel's agentic loop from stopping prematurely. Intercepts terminal FSM states (DONE/ERRS) and nudges the LLM to review the conversation and verify task completion before actually stopping. Resets after every successful tool call.

### tempo-c-cpp.el
C/C++ code template system using Emacs tempo. Provides quick insertion of common patterns (class definitions, function stubs, include guards, etc.).

## Prerequisites

### System Tools
- **clangd**: C/C++ language server (configured with `--background-index --clang-tidy`)
- **ripgrep** (`rg`): Fast searching
- **tree**: Directory listing (used by gptel-agent glob)
- **git**: Version control
- **aspell**: Spell checking (American English)

### Emacs Version
- **Emacs 30.1** (required for current elpa directory structure)
- Tree-sitter support (built-in since Emacs 29)

## Installation

1. Clone to `~/.emacs.d/`
2. Start Emacs — packages auto-install on first launch
3. For AI features: configure your LLM backend in `init-gptel.el`

## Author

Huming Chen <chenhuming@gmail.com>
- GitHub: [beacoder](https://github.com/beacoder/dot-emacs)
