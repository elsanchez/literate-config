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

;; Load test runner
(let ((test-runner-file "~/org/literate-config/test-examples/test-runner.el"))
  (when (file-exists-p (expand-file-name test-runner-file))
    (load-file (expand-file-name test-runner-file))
    (message "✓ Test runner loaded")))

;; Setup keybindings
(global-set-key (kbd "C-c t") 'test-runner-menu)
(global-set-key (kbd "C-c C-t") 'test-runner)

;; Message
(message "🧪 Test profile loaded - Press C-c t for test menu")
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

;; Load test runner for script menu examples
(let ((test-runner-file "~/org/literate-config/test-examples/test-runner.el"))
  (when (file-exists-p (expand-file-name test-runner-file))
    (load-file (expand-file-name test-runner-file))))

;; Setup keybindings
(map! :leader
      (:prefix ("t" . "Test")
       :desc "Test runner menu" "t" #'test-runner-menu
       :desc "Linus scripts" "l" #'test-linus-scripts
       :desc "Stallman scripts" "s" #'test-stallman-scripts
       :desc "Magit enhanced" "m" #'test-magit-enhanced-scripts
       :desc "Python runner" "p" #'test-python-runner))

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

case $IMPL in
    linus|l)
        echo "🔧 Testing Linus implementation..."
        emacs --batch --load ~/org/literate-config/test-examples/test-runner.el --eval "(test-linus-scripts)"
        ;;
    stallman|s)
        echo "📚 Testing Stallman implementation..."
        emacs --batch --load ~/org/literate-config/test-examples/test-runner.el --eval "(test-stallman-scripts)"
        ;;
    magit|m)
        echo "⚡ Testing Magit Enhanced implementation..."
        emacs --batch --load ~/org/literate-config/test-examples/test-runner.el --eval "(test-magit-enhanced-scripts)"
        ;;
    python|p)
        echo "🐍 Testing Python implementation..."
        ~/org/literate-config/examples/menus/script_runner.py --config ~/org/literate-config/test-examples/configs/test-runner.yaml
        ;;
    *)
        echo "Usage: $0 [linus|stallman|magit|python]"
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
./quick-test.sh python

# Launch test environments
./launch-emacs-test.sh
./launch-doom-test.sh
```

### Interactive Testing
```emacs-lisp
;; In Emacs, load test runner
(load-file "~/org/literate-config/test-examples/test-runner.el")

;; Test individual implementations
(test-linus-scripts)
(test-stallman-scripts)
(test-magit-enhanced-scripts)
(test-python-runner)

;; Setup test environment
(test-runner-setup-test-environment)

;; Main test menu
(test-runner-menu)
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

### Python Implementation
- Rich TUI interface
- YAML configuration
- Cross-platform compatibility
- Argument validation
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
    echo "   ./quick-test.sh linus     # Quick test specific implementation"
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
    rm -f "$SCRIPT_DIR/launch-"*.sh
    rm -f "$SCRIPT_DIR/quick-test.sh"
    rm -f "$SCRIPT_DIR/TEST_CASES.md"
    
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