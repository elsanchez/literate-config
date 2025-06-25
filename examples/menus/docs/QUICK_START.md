# Quick Start Guide

## 🚀 Get Started in 5 Minutes

### Step 1: Choose Your Approach

**For Testing/Comparison**: Use the testing framework
**For Production Use**: Pick an implementation directly

### Step 2: Testing Framework (Recommended for Exploration)

```emacs-lisp
;; Load the comprehensive testing framework
(load-file "../../test-examples/test-runner.el")

;; Create sample scripts for testing
(test-runner-setup-test-environment)

;; Open interactive test menu
(test-runner-menu)
```

**In the test menu:**
- `l` - Test Linus Scripts (pragmatic)
- `s` - Test Stallman Scripts (comprehensive)
- `m` - Test Magit Enhanced (visual)
- `p` - Test Python TUI

### Step 3: Direct Implementation Usage

#### Linus Scripts (Fast & Direct)

```emacs-lisp
(load-file "examples/menus/linustorv.el")
(linus-scripts)
```

**Keybindings:**
- `r` - Run script
- `e` - Edit script  
- `n` - New script
- `d` - Open directory
- `g` - Git operations submenu

#### Stallman Scripts (Comprehensive)

```emacs-lisp
(load-file "examples/menus/stallman.el")
(stallman-scripts)
```

**Features:**
- Extensive help system (`h` key)
- Full customization support
- Detailed documentation
- Freedom-focused design

#### Magit Enhanced (Visual)

```emacs-lisp
(load-file "examples/menus/magit-enhanced.el")
(magit-enhanced-scripts)
```

**Features:**
- Auto-discovery of scripts
- Visual organization by tags
- Template-based script creation
- Rich metadata display

#### Python TUI (Cross-Platform)

```bash
# Make executable
chmod +x examples/menus/script_runner.py

# Auto-discover scripts
./examples/menus/script_runner.py --discover

# Run with rich interface
./examples/menus/script_runner.py
```

## 🎯 Recommended Learning Path

### 1. Start with Testing Framework

```emacs-lisp
;; Complete setup for exploration
(progn
  (load-file "../../test-examples/test-runner.el")
  (test-runner-setup-test-environment)
  (test-runner-menu))
```

### 2. Try Each Implementation

Test each approach to understand differences:
1. **Linus** - Note the speed and directness
2. **Stallman** - Explore the help system
3. **Magit** - See the visual organization
4. **Python** - Experience the rich TUI

### 3. Choose Your Favorite

Based on your workflow:
- **Speed**: Linus
- **Features**: Stallman  
- **Visual**: Magit
- **Cross-platform**: Python

### 4. Customize for Your Needs

Example customization:

```emacs-lisp
;; Customize script directory
(setq ts-script-dir "~/my-scripts")

;; Add to your config
(global-set-key (kbd "C-c s") 'linus-scripts)
```

## 📁 Script Organization

### Create Test Scripts

The testing framework creates these sample scripts:

```bash
~/org/literate-config/test-examples/scripts/
├── deploy.sh          # Deployment with arguments
├── test.sh            # Test runner
├── build.sh           # Build system
├── git-status.sh      # Git operations
├── git-cleanup.sh     # Branch cleanup
└── backup.sh          # File backup
```

### Script Metadata Format

Add metadata to your scripts for better integration:

```bash
#!/bin/bash
# Description: Brief description of what script does
# Tags: tag1, tag2, tag3
# Help: Detailed help text explaining usage
# @arg name: Argument description
# @arg count: Number of iterations (optional)

# Your script content here
```

## 🎮 Common Workflows

### Workflow 1: Quick Script Execution

1. Press keybinding (`C-c s` or similar)
2. Type `r` for run
3. Select script from list
4. Provide arguments if needed
5. Script executes

### Workflow 2: Script Discovery

1. Open script interface
2. Use discovery features:
   - **Linus**: `d` to browse directory
   - **Stallman**: `d` for directory browser
   - **Magit**: Auto-discovery with tags
   - **Python**: Rich TUI exploration

### Workflow 3: Script Creation

1. Open interface
2. Press `n` for new script
3. Choose template (if available)
4. Edit script content
5. Make executable
6. Test execution

## 🔧 Configuration Examples

### Minimal Setup

```emacs-lisp
;; Load preferred implementation
(load-file "examples/menus/linustorv.el")

;; Set script directory
(setq ts-script-dir "~/scripts")

;; Bind to convenient key
(global-set-key (kbd "C-c x") 'linus-scripts)
```

### Advanced Setup

```emacs-lisp
;; Load multiple implementations
(load-file "examples/menus/stallman.el")
(load-file "examples/menus/magit-enhanced.el")

;; Configure directories
(setq stallman-scripts-directory "~/scripts")
(setq me-config-file "~/.config/script-menus.json")

;; Create menu for choosing implementation
(defun my/script-menu ()
  "Choose script implementation."
  (interactive)
  (let ((choice (completing-read "Implementation: " 
                                '("Stallman" "Magit Enhanced"))))
    (pcase choice
      ("Stallman" (stallman-scripts))
      ("Magit Enhanced" (magit-enhanced-scripts)))))

(global-set-key (kbd "C-c s") 'my/script-menu)
```

## 🐛 Troubleshooting

### Common Issues

**Error: `void-function defstruct`**
```emacs-lisp
;; Solution: Add cl-lib requirement
(require 'cl-lib)
```

**Error: `transient not found`**
```emacs-lisp
;; Solution: Install transient package
(package-install 'transient)
```

**Scripts not discovered**
- Check script directory exists
- Ensure scripts are executable (`chmod +x script.sh`)
- Verify script has proper shebang (`#!/bin/bash`)

### Performance Issues

**Slow discovery:**
- Enable caching in implementations that support it
- Reduce number of directories searched
- Use .gitignore patterns to exclude files

**Memory usage:**
- Clear caches periodically
- Limit metadata parsing depth
- Use lazy loading where available

## 📈 Next Steps

1. **Read [IMPLEMENTATIONS.md](IMPLEMENTATIONS.md)** for detailed implementation guide
2. **Explore [TESTING.md](TESTING.md)** for testing framework
3. **Check [API.md](API.md)** for extension development
4. **Contribute** improvements back to the project

## 🎯 Pro Tips

- **Use testing framework first** to understand differences
- **Customize keybindings** to match your workflow  
- **Add script metadata** for better organization
- **Start simple** and add complexity gradually
- **Share configurations** with your team

---

**Ready to start?** Run the testing framework and explore all implementations!