# Quick Start Guide - Script Menu Testing

## 🚀 Immediate Testing

### Option 1: Load and Test in Current Emacs
```emacs-lisp
;; Load the test runner
(load-file "~/org/literate-config/test-examples/test-runner.el")

;; Setup test environment (creates sample scripts)
(test-runner-setup-test-environment)

;; Open main test menu
(test-runner-menu)

;; Or test specific implementations directly:
(test-linus-scripts)       ; 🔧 Pragmatic approach
(test-stallman-scripts)    ; 📚 Freedom-focused
(test-magit-enhanced-scripts) ; ⚡ Visual interface
(test-python-runner)       ; 🐍 Cross-platform TUI
```

### Option 2: Isolated Test Environment
```bash
# Setup isolated test profiles
cd ~/org/literate-config/test-examples
./setup-test-profile.sh

# Launch Emacs with clean test profile
./launch-emacs-test.sh

# Or launch Doom Emacs with test profile
./launch-doom-test.sh

# Quick command-line tests
./quick-test.sh linus
./quick-test.sh stallman
./quick-test.sh magit
./quick-test.sh python
```

## 🎯 What Each Test Includes

### Sample Scripts Created
- **deploy.sh** - Application deployment with environment/version args
- **test.sh** - Test runner with type/coverage options
- **build.sh** - Build system with target/clean flags
- **git-status.sh** - Enhanced git status display
- **git-cleanup.sh** - Branch cleanup with dry-run option
- **backup.sh** - File backup with compression option

### Test Scenarios
1. **Basic menu navigation**
2. **Script execution with arguments**
3. **Help system exploration**
4. **Error handling**
5. **Configuration options**

## 🔧 Implementation Differences

### Linus Scripts (`test-linus-scripts`)
- **Style**: Direct, no-nonsense
- **Keys**: `C-c x` or call `(linus-scripts)`
- **Features**: Quick execution, Git submenu, minimal config
- **Philosophy**: "Just works"

### Stallman Scripts (`test-stallman-scripts`)
- **Style**: Comprehensive, freedom-focused
- **Keys**: `C-c s` or call `(stallman-scripts)`
- **Features**: Extensive help, customization, documentation
- **Philosophy**: User freedom and transparency

### Magit Enhanced (`test-magit-enhanced-scripts`)
- **Style**: Visual, feature-rich
- **Keys**: `C-c M-s` or call `(magit-enhanced-scripts)`
- **Features**: Auto-discovery, templates, tag organization
- **Philosophy**: Visual exploration and discovery

### Python Runner (`test-python-runner`)
- **Style**: Cross-platform TUI
- **Command**: `./script_runner.py`
- **Features**: YAML config, rich interface, argument validation
- **Philosophy**: Modern TUI with configuration

## 🎮 Interactive Testing Commands

### Test Runner Menu (`C-c t` in test profile)
```
┌─ Test Implementations ─┐  ┌─ Setup & Management ─┐
│ l - Linus Scripts      │  │ L - Load examples     │
│ s - Stallman Scripts   │  │ S - Setup environment │
│ m - Magit Enhanced     │  │ d - Scripts directory │
│ p - Python Runner      │  │ c - Configs directory │
└────────────────────────┘  └───────────────────────┘
```

### Directory Structure
```
test-examples/
├── test-runner.el          # Main test runner
├── setup-test-profile.sh   # Environment setup
├── scripts/               # Sample scripts (auto-created)
├── configs/               # Configuration files
├── profiles/              # Test profiles
└── QUICK_START.md         # This file
```

## 🚀 One-Line Starters

```bash
# Complete setup + Emacs launch
cd ~/org/literate-config/test-examples && ./setup-test-profile.sh && ./launch-emacs-test.sh

# Quick Linus test
emacs --eval "(progn (load-file \"~/org/literate-config/test-examples/test-runner.el\") (test-runner-setup-test-environment) (test-linus-scripts))"

# Current Emacs test
M-: (progn (load-file "~/org/literate-config/test-examples/test-runner.el") (test-runner-menu))
```

## 🔍 What to Look For

### User Experience
- **Navigation**: How intuitive is menu navigation?
- **Help**: How accessible is help and documentation?
- **Feedback**: How clear are status messages and errors?
- **Speed**: How responsive is the interface?

### Functionality
- **Script Discovery**: Does it find scripts correctly?
- **Argument Handling**: Are script arguments collected properly?
- **Execution**: Do scripts run as expected?
- **Error Handling**: How are errors reported?

### Visual Design
- **Layout**: Is the interface well-organized?
- **Colors**: Are visual cues helpful?
- **Information**: Is the right amount of detail shown?
- **Consistency**: Are interactions predictable?

## 🎨 Customization Examples

Each implementation can be customized:

```emacs-lisp
;; Linus Scripts
(setq ts-script-dir "~/my-scripts")
(setq ts-async nil)  ; Synchronous execution

;; Stallman Scripts  
(setq stallman-scripts-directory "~/automation")
(setq stallman-scripts-confirm-execution t)

;; Magit Enhanced
(setq me-config-file "~/.config/my-scripts.json")
(setq me-templates-dir "~/script-templates")
```

Ready to explore? Start with `(test-runner-menu)` or run the setup script!