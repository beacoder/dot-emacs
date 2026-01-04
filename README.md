# dot-emacs

A comprehensive Emacs configuration focused on C++ development with modern tooling and productivity enhancements.

## Overview

This Emacs configuration provides a feature-rich development environment optimized for C++ programming while also supporting a wide range of other languages and workflows. The configuration is modular, well-organized, and designed for productivity.

## Key Features

### C++ Development
- **Modern C++ tooling**: Tree-sitter support (c-ts-mode, c++-ts-mode) for Emacs 29+
- **Code navigation**: Ggtags integration with GNU Global
- **Code style**: Google C++ style support with automatic formatting
- **Build integration**: Compilation and error handling with flymake
- **Header/source navigation**: Quick switching between `.h` and `.cpp` files
- **Code completion**: Corfu for in-buffer completion

### Language Server Protocol (LSP)
- **Eglot integration**: Lightweight LSP client for multiple languages
- **Language support**: C/C++, Python, JavaScript, TypeScript, Rust, Go, Java, and more

### Modern Editor Features
- **Tree-sitter**: Built-in syntax tree parsing (Emacs 29+)
- **Completion**: Corfu for popup completion, Ivy/Counsel for fuzzy finding
- **Syntax highlighting**: Enhanced with symbol-overlay and rainbow-delimiters
- **Code navigation**: Avy for quick cursor movement, expand-region for text selection
- **Multiple cursors**: Edit multiple locations simultaneously

### AI Integration
- **gptel**: ChatGPT/OpenAI integration within Emacs
- **AI-assisted coding**: Code generation and assistance directly in the editor

### Development Tools
- **Version control**: Magit for Git operations, diff-hl for inline diffs
- **Project management**: Projectile for project navigation
- **Debugging**: GUD integration for debugging
- **Documentation**: Eldoc for inline documentation

### Productivity Enhancements
- **Window management**: EXWM integration (Linux window manager)
- **Session management**: Save and restore editor sessions
- **Note-taking**: Org-mode with extensive customization
- **File management**: Dired with preview capabilities
- **Search tools**: Ripgrep integration with counsel

### Visual Customization
- **Theme**: Zenburn color theme
- **UI enhancements**: Smart mode-line, which-key for keybindings
- **Fonts**: Configurable font settings with variable pitch for prose

## Directory Structure

```
.dot-emacs/
├── .emacs.d/
│   ├── init.el              # Main entry point
│   ├── elisp/               # Modular configuration files
│   │   ├── init-3rd-party.el    # Third-party package configurations
│   │   ├── init-basics.el       # Basic Emacs settings
│   │   ├── init-cc.el           # C/C++ development settings
│   │   ├── init-eglot.el        # LSP configuration
│   │   ├── init-elpa.el         # Package management
│   │   ├── init-org.el          # Org-mode configuration
│   │   ├── init-gptel.el        # AI integration
│   │   └── ... (30+ more config files)
│   ├── site-lisp/           # Local packages
│   ├── treesit-grammars/    # Tree-sitter grammars
│   └── tutorials/           # Learning resources
├── .gnus                    # Email configuration
└── README.md               # This file
```

## Installation

### Quick Start

1. **Clone the repository**:
   ```bash
   git clone https://github.com/beacoder/dot-emacs.git ~/.emacs.d
   ```

2. **Backup existing configuration** (if any):
   ```bash
   mv ~/.emacs ~/.emacs.backup  # If you have an existing .emacs file
   mv ~/.emacs.d ~/.emacs.d.backup  # If you have an existing .emacs.d directory
   ```

3. **Create symbolic links**:
   ```bash
   ln -sf ~/.emacs.d/.emacs.d ~/.emacs.d
   ```

4. **Start Emacs**: The first launch will automatically download and install all required packages.

### Manual Installation

If you prefer to keep your configuration separate:

1. Copy the `.emacs.d` directory to your preferred location
2. Add the following to your `~/.emacs` file:
   ```elisp
   (load-file "~/.emacs.d/init.el")
   ```

## Configuration Files

The configuration is organized into modular files:

### Core Configuration
- `init.el` - Main entry point, sets up load paths and loads modules
- `init-basics.el` - Fundamental Emacs settings and keybindings
- `init-elpa.el` - Package management with use-package

### Development Environments
- `init-cc.el` - C/C++ development with modern tooling
- `init-python.el` - Python development setup
- `init-javascript.el` - JavaScript/TypeScript configuration
- `init-ruby.el` - Ruby development tools
- `init-org.el` - Org-mode for notes, tasks, and documentation

### Tool Integration
- `init-eglot.el` - Language Server Protocol (LSP) client
- `init-git.el` - Git integration with Magit
- `init-compile.el` - Build and compilation tools
- `init-gptel.el` - AI assistant integration

### UI and Productivity
- `init-ui.el` - Visual customization and themes
- `init-ivy.el` - Fuzzy finding and completion
- `init-windows.el` - Window management and layout
- `init-productivity.el` - Productivity tools and workflows

## Key Bindings

### General Navigation
- `M-.` - Find definition (ggtags-find-tag-dwim)
- `M-]` - Find references
- `C-c o` - Switch between header and source files
- `M-:` - Avy goto character
- `M-8` - Expand region

### Search and Find
- `C-s` / `C-r` - Incremental search
- `M-s` - Project search with ag
- `C-x q a` - Counsel ag (ripgrep)
- `C-x q g` - Counsel git grep
- `C-x q f` - Counsel git find file

### Code Operations
- `C-c C-c` - Compile current project
- `C-c C-e` - Run tests
- `C-x u` - Undo tree visualization
- `C-M-y` - Browse kill ring

### Window Management
- `C-x 2` / `C-x 3` - Split windows
- `C-x 0` / `C-x 1` - Close window/frame
- `C-x o` - Switch between windows

### Git Operations (Magit)
- `C-x g` - Open Magit status
- `s` - Stage changes
- `c` - Commit
- `p` - Push
- `P` - Pull

## Prerequisites

### System Tools
- **Git**: For version control and package management
- **GNU Global**: For C/C++ tag generation (`gtags`)
- **ripgrep**: For fast searching (`rg`)
- **fd**: Alternative to find command
- **clangd**: C/C++ language server (recommended)

### Language Servers (for Eglot)
- **C/C++**: clangd or ccls
- **Python**: pylsp or pyright
- **JavaScript/TypeScript**: typescript-language-server
- **Rust**: rust-analyzer
- **Go**: gopls

### Tree-sitter (Emacs 29+)
- Install tree-sitter grammars for enhanced syntax highlighting:
  ```bash
  # Example for C/C++
  git clone https://github.com/tree-sitter/tree-sitter-c ~/.emacs.d/treesit-grammars/c
  git clone https://github.com/tree-sitter/tree-sitter-cpp ~/.emacs.d/treesit-grammars/cpp
  ```

## Customization

### Personal Settings
Create a file `~/.emacs.d/elisp/init-personal.el` for your customizations:
```elisp
;;; init-personal.el --- Personal configurations -*- lexical-binding: t -*-

;; Add your personal settings here
(setq user-full-name "Your Name"
      user-mail-address "your.email@example.com")

;; Additional packages
(when (maybe-require-package 'some-package)
  (require 'some-package))

;; Custom keybindings
(global-set-key (kbd "C-c p") 'some-command)
```

### Theme Customization
Change the theme by modifying `init-ui.el` or adding to your personal config:
```elisp
(load-theme 'modus-vivendi t)  ;; Dark theme
;; or
(load-theme 'modus-operandi t) ;; Light theme
```

## Troubleshooting

### Common Issues

1. **Package installation fails**:
   - Check your internet connection
   - Ensure ELPA repositories are accessible (may require proxy in some regions)
   - Try running `M-x package-refresh-contents`

2. **Tree-sitter not working**:
   - Ensure you're using Emacs 29+
   - Install tree-sitter grammars in `~/.emacs.d/treesit-grammars/`
   - Check `(treesit-available-p)` returns `t`

3. **Language server not starting**:
   - Verify the language server is installed and in your PATH
   - Check `eglot` server logs with `M-x eglot-events-buffer`
   - Ensure project has appropriate configuration files (e.g., `compile_commands.json` for C/C++)

4. **Performance issues**:
   - Disable heavy modes with `M-x profiler-start`
   - Check `init-benchmarking.el` for performance measurement tools
   - Consider using `so-long` mode for large files

### Debugging
- `M-x toggle-debug-on-error` - Enable debug on error
- `M-x view-echo-area-messages` - View recent messages
- `M-x profiler-report` - Profile performance
- Check `*Messages*` buffer for errors and warnings

## Contributing

Feel free to fork this repository and adapt it to your needs. If you have improvements:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## License

This configuration is shared under the MIT License. See the LICENSE file for details.

## Acknowledgments

- Inspired by various Emacs configurations including Purcell's, Doom, and Spacemacs
- Thanks to the Emacs community for countless packages and tutorials
- Special thanks to package maintainers for their work

## Contact

For questions or suggestions:
- GitHub Issues: [https://github.com/beacoder/dot-emacs/issues](https://github.com/beacoder/dot-emacs/issues)
- Email: Check the repository for contact information

---

*Happy coding with Emacs!*
