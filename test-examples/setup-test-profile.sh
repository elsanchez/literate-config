#!/bin/bash
# Setup script for creating isolated test profile for script menu examples

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="$HOME/org/literate-config"
TEST_PROFILE_DIR="$HOME/.config/emacs-test-profile"
DOOM_TEST_DIR="$HOME/.config/doom-test"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

error() {
    echo -e "${RED}✗${NC} $1"
}

# Function to create test profile
create_test_profile() {
    log "Creating test profile directory: $TEST_PROFILE_DIR"
    
    # Create directory structure
    mkdir -p "$TEST_PROFILE_DIR"
    mkdir -p "$DOOM_TEST_DIR"
    
    # Create minimal init.el for test profile
    cat > "$TEST_PROFILE_DIR/init.el" << 'EOF'
;;; Test Profile init.el

;; Minimal Emacs configuration for testing script menus
(setq user-emacs-directory (file-name-directory load-file-name))

;; Package setup
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install required packages if not present
(unless (package-installed-p 'transient)
  (package-refresh-contents)
  (package-install 'transient))

;; Load fix-transient first
(let ((fix-file "~/org/literate-config/test-examples/fix-transient.el"))
  (when (file-exists-p (expand-file-name fix-file))
    (load-file (expand-file-name fix-file))))

;; Load simple loader
(let ((loader-file "~/org/literate-config/test-examples/simple-loader.el"))
  (when (file-exists-p (expand-file-name loader-file))
    (load-file (expand-file-name loader-file))
    (message "✓ Simple loader available")))

;; Setup keybindings
(global-set-key (kbd "C-c t") 'simple-loader-menu)
(global-set-key (kbd "C-c h") 'simple-loader-load-hybrid)
(global-set-key (kbd "C-c b") 'simple-loader-load-basic)

;; Message
(message "🧪 Test profile loaded - Press C-c t for menu")
EOF
    
    success "Test profile created at $TEST_PROFILE_DIR"
}

# Function to create Doom test configuration
create_doom_test_config() {
    log "Creating Doom test configuration"
    
    # Create minimal Doom config for testing
    cat > "$DOOM_TEST_DIR/init.el" << 'EOF'
;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       vertico

       :ui
       doom
       doom-dashboard
       modeline
       ophints
       (popup +defaults)
       treemacs
       vc-gutter
       vi-tilde-fringe
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       snippets

       :emacs
       dired
       electric
       undo
       vc

       :term
       vterm

       :checkers
       syntax

       :tools
       magit
       lookup
       lsp

       :lang
       emacs-lisp
       markdown
       org
       sh

       :config
       (default +bindings +smartparens))
EOF

    cat > "$DOOM_TEST_DIR/config.el" << 'EOF'
;;; config.el -*- lexical-binding: t; -*-

;; Ensure transient is available in Doom
(when (fboundp 'straight-use-package)
  (straight-use-package 'transient))

(require 'transient)
(require 'cl-lib)

;; Load fix for transient issues
(let ((fix-file "~/org/literate-config/test-examples/fix-transient.el"))
  (when (file-exists-p (expand-file-name fix-file))
    (load-file (expand-file-name fix-file))))

;; Load simple loader
(let ((loader-file "~/org/literate-config/test-examples/simple-loader.el"))
  (when (file-exists-p (expand-file-name loader-file))
    (load-file (expand-file-name loader-file))))

;; Setup keybindings
(map! :leader
      (:prefix ("t" . "🧪 Test")
       :desc "Test menu" "t" #'simple-loader-menu
       :desc "Load basic" "b" #'simple-loader-load-basic
       :desc "Load hybrid" "h" #'simple-loader-load-hybrid
       :desc "Load individual" "i" #'simple-loader-load-individual
       :desc "Fix transient" "f" #'fix-doom-transient))

(message "🧪 Doom test profile loaded - Use SPC t for tests")
EOF

    cat > "$DOOM_TEST_DIR/packages.el" << 'EOF'
;; -*- no-byte-compile: t; -*-
;;; packages.el

;; Required for script menus
(package! transient)
EOF
    
    success "Doom test configuration created at $DOOM_TEST_DIR"
}

# Function to create launcher scripts
create_launchers() {
    log "Creating launcher scripts"
    
    # Create Emacs test launcher
    cat > "$SCRIPT_DIR/launch-emacs-test.sh" << EOF
#!/bin/bash
# Launch Emacs with test profile

echo "🧪 Launching Emacs with test profile..."
echo "📁 Profile: $TEST_PROFILE_DIR"
echo "🎯 Press C-c t for test menu"
echo ""

emacs --init-directory="$TEST_PROFILE_DIR" "\$@"
EOF
    chmod +x "$SCRIPT_DIR/launch-emacs-test.sh"
    
    # Create Doom test launcher
    cat > "$SCRIPT_DIR/launch-doom-test.sh" << EOF
#!/bin/bash
# Launch Doom Emacs with test profile

if ! command -v doom &> /dev/null; then
    echo "❌ Doom Emacs not found. Please install Doom first."
    exit 1
fi

echo "🧪 Launching Doom Emacs with test profile..."
echo "📁 Profile: $DOOM_TEST_DIR"
echo "🎯 Press SPC t for test menu"
echo ""

# Set DOOMDIR and launch
export DOOMDIR="$DOOM_TEST_DIR"
emacs "\$@"
EOF
    chmod +x "$SCRIPT_DIR/launch-doom-test.sh"
    
    # Create quick test script
    cat > "$SCRIPT_DIR/quick-test.sh" << 'EOF'
#!/bin/bash
# Quick test of a specific implementation

IMPL=${1:-linus}
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case $IMPL in
    linus|l)
        echo "🔧 Testing Linus implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-individual \"linus\")"
        ;;
    stallman|s)
        echo "📚 Testing Stallman implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-individual \"stallman\")"
        ;;
    magit|m)
        echo "⚡ Testing Magit Enhanced implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-individual \"magit\")"
        ;;
    hybrid|h)
        echo "🧬 Testing Hybrid implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-hybrid)"
        ;;
    basic|b)
        echo "🧪 Testing Basic framework..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-basic)"
        ;;
    python|p)
        echo "🐍 Testing Python implementation..."
        PYTHON_SCRIPT="$BASE_DIR/../examples/menus/script_runner.py"
        if [ -f "$PYTHON_SCRIPT" ]; then
            python3 "$PYTHON_SCRIPT" --config "$BASE_DIR/configs/test-runner.yaml"
        else
            echo "❌ Python script not found"
        fi
        ;;
    *)
        echo "Usage: $0 [linus|stallman|magit|hybrid|basic|python]"
        echo ""
        echo "Available implementations:"
        echo "  linus     - Linus Torvalds style (fast, pragmatic)"
        echo "  stallman  - Richard Stallman style (comprehensive)"
        echo "  magit     - Magit Enhanced style (visual)"
        echo "  hybrid    - Hybrid implementation (best UX)"
        echo "  basic     - Basic test framework"
        echo "  python    - Python TUI version"
        exit 1
        ;;
esac
EOF
    chmod +x "$SCRIPT_DIR/quick-test.sh"
    
    success "Launcher scripts created"
}

# Function to create sample test cases
create_test_cases() {
    log "Creating test case documentation"
    
    cat > "$SCRIPT_DIR/TEST_CASES.md" << 'EOF'
# Script Menu Test Cases

## Test Scenarios

### 1. Basic Functionality
- [ ] Load implementation without errors
- [ ] Display main menu
- [ ] Navigate through menu options
- [ ] Execute simple script
- [ ] Handle script with arguments

### 2. Script Discovery
- [ ] Auto-discover scripts in directory
- [ ] Parse script metadata correctly
- [ ] Handle scripts without metadata
- [ ] Filter scripts by tags

### 3. Argument Handling
- [ ] Collect required arguments
- [ ] Handle optional arguments
- [ ] Validate argument choices
- [ ] Display help for arguments

### 4. Error Handling
- [ ] Handle missing scripts gracefully
- [ ] Show meaningful error messages
- [ ] Recover from script execution errors
- [ ] Handle invalid arguments

### 5. User Experience
- [ ] Intuitive navigation
- [ ] Clear help documentation
- [ ] Responsive interface
- [ ] Consistent keybindings

## Manual Testing

### Quick Test Commands
```bash
# Test specific implementation
./quick-test.sh linus
./quick-test.sh stallman
./quick-test.sh magit
./quick-test.sh hybrid
./quick-test.sh basic
./quick-test.sh python

# Launch test environments
./launch-emacs-test.sh
./launch-doom-test.sh
```

### Interactive Testing
```emacs-lisp
;; Load simple loader
(load-file "simple-loader.el")

;; Test menu
(simple-loader-menu)

;; Test individual implementations
(simple-loader-load-individual "linus")
(simple-loader-load-individual "stallman")
(simple-loader-load-individual "magit")

;; Test hybrid implementation
(simple-loader-load-hybrid)

;; Test basic framework
(simple-loader-load-basic)
```

## Expected Behaviors

### Linus Implementation
- Direct, no-nonsense interface
- Quick script execution
- Minimal configuration
- Git submenu available

### Stallman Implementation
- Comprehensive help system
- Detailed documentation
- Freedom-focused messaging
- Customization options

### Magit Enhanced Implementation
- Visual menu exploration
- Dynamic script discovery
- Template-based creation
- Tag-based organization

### Hybrid Implementation
- Intelligent script suggestions
- Smart search functionality
- Performance-optimized caching
- Best UX from all approaches

### Basic Framework
- Test environment setup
- Sample script creation
- Interactive testing menu
- Comparison capabilities

## Troubleshooting

### Common Issues
- `transient-define-prefix` not found: Use fix-transient.el
- Scripts not discovered: Check script directory and permissions
- Performance issues: Enable caching, reduce scan directories

### Solutions
```bash
# Fix transient issues
emacs --load fix-transient.el --eval "(fix-doom-transient)"

# Reset test environment
rm -rf ~/.config/emacs-test-profile
./setup-test-profile.sh

# Clean start
./quick-test.sh basic
```
EOF

    success "Test cases documentation created"
}

# Function to setup test environment
setup_test_environment() {
    log "Setting up complete test environment"
    
    create_test_profile
    create_doom_test_config
    create_launchers
    create_test_cases
    
    # Create symlink for easy access
    ln -sf "$SCRIPT_DIR" "$HOME/.script-menu-tests" 2>/dev/null || true
    
    success "Test environment setup complete!"
    echo ""
    echo "🎯 Quick Start:"
    echo "   ./launch-emacs-test.sh    # Launch Emacs with test profile"
    echo "   ./launch-doom-test.sh     # Launch Doom with test profile"
    echo "   ./quick-test.sh basic     # Quick test basic framework"
    echo "   ./quick-test.sh hybrid    # Quick test hybrid implementation"
    echo ""
    echo "📖 Test Cases: $SCRIPT_DIR/TEST_CASES.md"
    echo "🔗 Symlink: ~/.script-menu-tests -> $SCRIPT_DIR"
}

# Function to clean test environment
clean_test_environment() {
    log "Cleaning test environment"
    
    rm -rf "$TEST_PROFILE_DIR"
    rm -rf "$DOOM_TEST_DIR"
    rm -f "$HOME/.script-menu-tests"
    
    success "Test environment cleaned"
}

# Main execution
case "${1:-setup}" in
    setup)
        setup_test_environment
        ;;
    clean)
        clean_test_environment
        ;;
    profile)
        create_test_profile
        ;;
    doom)
        create_doom_test_config
        ;;
    launchers)
        create_launchers
        ;;
    *)
        echo "Usage: $0 [setup|clean|profile|doom|launchers]"
        echo ""
        echo "Commands:"
        echo "  setup     - Setup complete test environment (default)"
        echo "  clean     - Clean all test files"
        echo "  profile   - Create Emacs test profile only"
        echo "  doom      - Create Doom test config only"
        echo "  launchers - Create launcher scripts only"
        exit 1
        ;;
esac