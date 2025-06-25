# Script Menu Examples Package

## 📦 Package Overview

This package provides multiple implementations of script management interfaces for Emacs, each with different philosophies and approaches. It includes comprehensive testing and evaluation frameworks.

## 📁 Package Structure

```
examples/menus/
├── linustorv.el              # Linus Torvalds style implementation
├── stallman.el               # Richard Stallman style implementation  
├── magit-enhanced.el         # Advanced Magit-style interface
├── script_runner.py          # Python TUI alternative
└── docs/                     # 📚 Documentation (you are here)
    ├── README.md             # This file
    ├── QUICK_START.md        # Quick start guide
    ├── IMPLEMENTATIONS.md    # Detailed implementation guide
    ├── TESTING.md            # Testing framework documentation
    └── API.md                # API reference
```

## 🚀 Quick Start

### Option 1: Test Individual Implementations

```emacs-lisp
;; Load and test Linus implementation (pragmatic approach)
(load-file "examples/menus/linustorv.el")
(linus-scripts)

;; Load and test Stallman implementation (comprehensive approach)  
(load-file "examples/menus/stallman.el")
(stallman-scripts)

;; Load and test Magit Enhanced (visual approach)
(load-file "examples/menus/magit-enhanced.el")  
(magit-enhanced-scripts)
```

### Option 2: Use Testing Framework

```emacs-lisp
;; Load comprehensive testing framework from test-examples/
(load-file "../../test-examples/test-runner.el")
(test-runner-setup-test-environment)
(test-runner-menu)  ; Interactive menu to test all implementations
```

### Option 3: Python TUI Alternative

```bash
# Make executable and run
chmod +x examples/menus/script_runner.py
./examples/menus/script_runner.py --discover
```

## 🎯 Implementation Comparison

| Feature | Linus | Stallman | Magit Enhanced | Python TUI |
|---------|-------|----------|----------------|------------|
| **Philosophy** | Pragmatic | Comprehensive | Visual | Cross-platform |
| **Performance** | ★★★★★ | ★★★☆☆ | ★★★☆☆ | ★★★★☆ |
| **Help System** | ★★☆☆☆ | ★★★★★ | ★★★☆☆ | ★★★☆☆ |
| **Customization** | ★★☆☆☆ | ★★★★★ | ★★★★☆ | ★★★★☆ |
| **Visual Design** | ★★☆☆☆ | ★★★☆☆ | ★★★★★ | ★★★★☆ |
| **Auto Discovery** | ★★☆☆☆ | ★★★☆☆ | ★★★★★ | ★★★★☆ |

## 🔧 Configuration

### Linus Scripts Configuration

```emacs-lisp
(setq ts-script-dir "~/bin")           ; Script directory
(setq ts-shell "/bin/bash")            ; Shell
(setq ts-async t)                      ; Async execution
```

### Stallman Scripts Configuration

```emacs-lisp
(setq stallman-scripts-directory "~/scripts")
(setq stallman-scripts-shell "/bin/bash")
(setq stallman-scripts-async-by-default t)
(setq stallman-scripts-confirm-execution nil)
```

### Magit Enhanced Configuration

```emacs-lisp
(setq me-config-file "~/.config/script-menus.json")
(setq me-templates-dir "~/.config/script-templates")
```

## 🎮 Usage Examples

### Basic Script Execution

All implementations support basic script execution:

1. **Open interface** (`linus-scripts`, `stallman-scripts`, etc.)
2. **Select script** (via menu or search)
3. **Provide arguments** (if required)
4. **Execute** (async or sync)

### Script with Arguments

Create scripts with metadata for better integration:

```bash
#!/bin/bash
# Description: Deploy application to environment
# Tags: deployment, ci-cd
# @arg environment: Target environment (dev/staging/prod)
# @arg version: Version to deploy (optional)

echo "Deploying to $1 with version ${2:-latest}"
```

### Advanced Features

- **Auto-discovery**: Automatically find scripts in configured directories
- **Metadata parsing**: Extract descriptions, arguments, and tags from comments
- **Tag organization**: Group scripts by functionality
- **Template support**: Create new scripts from templates
- **Error handling**: Graceful error recovery and reporting

## 📚 Documentation

- **[QUICK_START.md](QUICK_START.md)** - Get started in 5 minutes
- **[IMPLEMENTATIONS.md](IMPLEMENTATIONS.md)** - Detailed implementation guide
- **[TESTING.md](TESTING.md)** - Testing framework documentation
- **[API.md](API.md)** - API reference and extension guide

## 🧪 Testing Framework

This package includes a comprehensive testing framework located in `../../test-examples/`:

- **Unit tests** with ERT
- **Performance benchmarks**
- **UX evaluation scenarios**
- **Visual comparison dashboard**
- **Automated CI/CD testing**

See [TESTING.md](TESTING.md) for complete testing documentation.

## 🛠️ Extension Guide

### Adding New Implementations

1. Create new `.el` file following existing patterns
2. Implement core functions: script discovery, execution, help
3. Add to testing framework in `test-examples/`
4. Update documentation

### Custom Script Metadata

Support custom metadata by adding comment parsing:

```bash
# Custom-Field: Your custom value
# @custom-arg name: Custom argument type
```

## 🤝 Contributing

1. Follow existing code style and patterns
2. Add comprehensive documentation
3. Include tests for new features
4. Update this documentation

## 📄 License

Each implementation follows different license approaches:
- **Linus style**: Pragmatic (do whatever)
- **Stallman style**: GPL v3+ (freedom focused)
- **Magit Enhanced**: Modern permissive
- **Python TUI**: MIT/Apache compatible

## 🔗 Related Projects

- **Magit**: Inspiration for transient interfaces
- **Hydra**: Alternative menu system
- **Doom Emacs**: Configuration framework
- **Rich**: Python TUI library

---

**Next Steps**: Read [QUICK_START.md](QUICK_START.md) to start using the implementations.