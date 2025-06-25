# Script Menu Examples - Execution Guide

## 🚀 Quick Execution Reference

This guide shows you exactly how to run each implementation and testing framework.

## 📁 Package Location

```bash
# Navigate to package directory
cd ~/org/literate-config/examples/menus/
```

## 🎯 Individual Implementations

### 1. Linus Scripts (Pragmatic & Fast)

```emacs-lisp
;; Load and run
(load-file "examples/menus/linustorv.el")
(linus-scripts)
```

**Key Features:**
- ⚡ Fast startup and execution
- 🎮 Direct keybindings (`r`, `e`, `n`, `d`)
- 🔧 Git operations submenu
- 💻 Terminal-optimized interface

**Best for:** Power users, command-line workflows, minimal setup

### 2. Stallman Scripts (Comprehensive & Educational)

```emacs-lisp
;; Load and run
(load-file "examples/menus/stallman.el")
(stallman-scripts)
```

**Key Features:**
- 📚 Extensive help system (`h` for help)
- ⚙️ Full customization support
- 🆓 Freedom-focused design
- 📖 Comprehensive documentation

**Best for:** New users, learning environments, teams with varied skills

### 3. Magit Enhanced (Visual & Feature-Rich)

```emacs-lisp
;; Load and run
(load-file "examples/menus/magit-enhanced.el")
(magit-enhanced-scripts)
```

**Key Features:**
- 🔍 Auto-discovery with metadata parsing
- 🏷️ Tag-based organization
- 📝 Template system for new scripts
- 🎨 Rich visual interface

**Best for:** Large script collections, visual users, complex workflows

### 4. Python TUI (Cross-Platform & Rich)

```bash
# Make executable and run
chmod +x examples/menus/script_runner.py
./examples/menus/script_runner.py

# With configuration
./examples/menus/script_runner.py --config my-config.yaml

# Auto-discover mode
./examples/menus/script_runner.py --discover
```

**Key Features:**
- 🌍 Cross-platform (Windows, macOS, Linux)
- 🎨 Rich TUI with colors and progress bars
- ⚙️ YAML-based configuration
- ✅ Argument validation and choices

**Best for:** Non-Emacs users, CI/CD integration, rich visual feedback

## 🧪 Testing Framework

### Basic Test Runner

```emacs-lisp
;; Load test framework
(load-file "../../test-examples/test-runner.el")

;; Create sample scripts
(test-runner-setup-test-environment)

;; Open interactive test menu
(test-runner-menu)
```

**Test Menu Options:**
- `l` - Test Linus Scripts
- `s` - Test Stallman Scripts  
- `m` - Test Magit Enhanced
- `p` - Test Python Runner
- `L` - Load all examples
- `S` - Setup test environment

### Advanced Testing Framework

```emacs-lisp
;; Load advanced framework
(load-file "../../test-examples/advanced-test-runner.el")

;; Open advanced menu
(atr-main-menu)
```

**Advanced Features:**
- 📊 Performance benchmarking
- 📈 UX evaluation scenarios
- 🎯 Comparative analysis
- 🤖 AI-generated recommendations

### Automated Test Suite

```emacs-lisp
;; Load automated tests
(load-file "../../test-examples/automated-test-suite.el")

;; Run all tests
(ats-run-all-tests)

;; Run specific test category
(ats-run-test-category "unit")        ; Unit tests
(ats-run-test-category "integration") ; Integration tests
(ats-run-test-category "stress")      ; Stress tests

;; Generate test report
(ats-generate-test-report)
```

### Visual Dashboard

```emacs-lisp
;; Load dashboard
(load-file "../../test-examples/visual-dashboard.el")

;; Create interactive dashboard
(vd-create-dashboard)

;; Start auto-refresh monitoring
(vd-start-auto-refresh)
```

**Dashboard Features:**
- 📊 Real-time performance metrics
- 📈 Visual comparison charts
- ⭐ User ratings and feedback
- 🤖 AI recommendations
- 📤 Export capabilities

### Hybrid Implementation (Best UX)

```emacs-lisp
;; Load optimal hybrid implementation
(load-file "../../test-examples/hybrid-implementation.el")

;; Create test environment
(load-file "../../test-examples/test-runner.el")
(test-runner-setup-test-environment)

;; Launch hybrid interface
(hybrid-scripts)
```

**Hybrid Features:**
- 🧠 Intelligent script suggestions
- 🔍 Smart search: "What do you want to do?"
- 🏷️ Auto-organization by tags
- ⚡ Performance-optimized caching
- 🎨 Modern UX design

## 🏖️ Sandbox Environments

### Doom Emacs Sandbox

```bash
# Create and launch Doom sandbox
cd ~/org/literate-config/test-examples/
./create-doom-sandbox.sh
./launch-doom-sandbox.sh
```

**In Doom Sandbox:**
- `SPC t t` - Main test menu
- `SPC t h` - Hybrid implementation (recommended)
- `SPC t d` - Visual dashboard
- `SPC t l/s/m` - Individual implementations

### Vanilla Emacs Profile

```bash
# Launch vanilla Emacs with script testing
cd ~/org/literate-config/test-examples/
./launch-profiles.sh vanilla
```

**In Vanilla Profile:**
- `C-c t` - Main test menu
- `C-c h` - Hybrid implementation
- `C-c d` - Visual dashboard
- `C-c l/s/m` - Individual implementations

### All-in-One Launcher

```bash
# Choose profile interactively
cd ~/org/literate-config/test-examples/
./launch-profiles.sh

# Available options:
# doom     - Doom Emacs sandbox
# vanilla  - Vanilla Emacs profile  
# current  - Load in current session
```

## 🎮 Recommended Exploration Path

### 1. Start with Test Framework

```emacs-lisp
;; Complete setup for exploration
(progn
  (load-file "../../test-examples/test-runner.el")
  (test-runner-setup-test-environment)
  (test-runner-menu))
```

### 2. Try Hybrid Implementation (Best UX)

```emacs-lisp
;; Best overall experience
(progn
  (load-file "../../test-examples/hybrid-implementation.el")
  (load-file "../../test-examples/test-runner.el")
  (test-runner-setup-test-environment)
  (hybrid-scripts))
```

### 3. Compare Individual Implementations

Test each to understand differences:
1. **Linus** - Note speed and directness
2. **Stallman** - Explore comprehensive help
3. **Magit** - Experience visual organization
4. **Python** - Try rich TUI interface

### 4. Explore Advanced Features

```emacs-lisp
;; Advanced benchmarking and analysis
(load-file "../../test-examples/advanced-test-runner.el")
(atr-benchmark-all)
(atr-create-comparison-dashboard)
```

## 🔧 Configuration Examples

### Basic Setup (Choose Your Favorite)

```emacs-lisp
;; Add to your Emacs config
(load-file "~/org/literate-config/examples/menus/linustorv.el")
(global-set-key (kbd "C-c s") 'linus-scripts)
(setq ts-script-dir "~/scripts")
```

### Multi-Implementation Setup

```emacs-lisp
;; Load multiple implementations
(load-file "~/org/literate-config/examples/menus/stallman.el")
(load-file "~/org/literate-config/examples/menus/magit-enhanced.el")

;; Create selection menu
(defun my/choose-script-interface ()
  "Choose script interface implementation."
  (interactive)
  (let ((choice (completing-read "Implementation: " 
                                '("Stallman" "Magit Enhanced"))))
    (pcase choice
      ("Stallman" (stallman-scripts))
      ("Magit Enhanced" (magit-enhanced-scripts)))))

(global-set-key (kbd "C-c s") 'my/choose-script-interface)
```

### Development Setup with Testing

```emacs-lisp
;; Development environment with testing
(defun my/setup-script-development ()
  "Setup script development environment."
  (interactive)
  (load-file "~/org/literate-config/test-examples/test-runner.el")
  (load-file "~/org/literate-config/test-examples/hybrid-implementation.el")
  (test-runner-setup-test-environment)
  (global-set-key (kbd "C-c s") 'hybrid-scripts)
  (global-set-key (kbd "C-c t") 'test-runner-menu)
  (message "Script development environment ready!"))

;; Run once to setup
(my/setup-script-development)
```

## 🐛 Troubleshooting

### Common Issues

**Error: `void-function defstruct`**
```emacs-lisp
;; Solution: Ensure cl-lib is loaded
(require 'cl-lib)
(load-file "examples/menus/hybrid-implementation.el")
```

**Error: `transient not found`**
```emacs-lisp
;; Solution: Install transient package
(unless (package-installed-p 'transient)
  (package-refresh-contents)
  (package-install 'transient))
```

**Scripts not found**
- Ensure scripts directory exists: `mkdir -p ~/scripts`
- Make scripts executable: `chmod +x ~/scripts/*.sh`
- Check script paths in configuration

**Performance issues**
- Enable caching where available
- Reduce directories being scanned
- Use `.gitignore` patterns to exclude files

### Getting Help

**In implementations:**
- **Linus**: `h` for help
- **Stallman**: `h` for comprehensive help, `?` for keybindings
- **Magit**: `h` for help system
- **Hybrid**: Built-in contextual help

**Testing framework:**
- `h` in test menu for help
- Check `docs/` directory for detailed documentation

## 📚 Next Steps

1. **Start with test framework** - Get familiar with all implementations
2. **Choose your favorite** - Pick based on your workflow needs  
3. **Customize configuration** - Adapt to your script organization
4. **Add to your config** - Integrate with your Emacs setup
5. **Extend functionality** - Add custom features using the APIs

## 🎯 One-Line Quick Starts

```emacs-lisp
;; Hybrid implementation (best UX)
(progn (load-file "../../test-examples/hybrid-implementation.el") (load-file "../../test-examples/test-runner.el") (test-runner-setup-test-environment) (hybrid-scripts))

;; Test framework exploration
(progn (load-file "../../test-examples/test-runner.el") (test-runner-setup-test-environment) (test-runner-menu))

;; Advanced benchmarking
(progn (load-file "../../test-examples/advanced-test-runner.el") (atr-main-menu))

;; Visual dashboard
(progn (load-file "../../test-examples/visual-dashboard.el") (vd-create-dashboard))
```

---

**Ready to start?** Pick any of the execution methods above and start exploring!