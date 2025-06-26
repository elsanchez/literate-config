#!/bin/bash
# Launch different profiles for script menu testing

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

show_help() {
    echo "🧪 Script Menu Testing Profiles"
    echo "==============================="
    echo ""
    echo "Usage: $0 [PROFILE]"
    echo ""
    echo "Available profiles:"
    echo "  doom     - Doom Emacs sandbox (with transient fixes)"
    echo "  vanilla  - Vanilla Emacs profile"
    echo "  current  - Load in current Emacs session"
    echo "  fix      - Fix transient issues in current session"
    echo ""
    echo "Examples:"
    echo "  $0 doom      # Launch Doom sandbox"
    echo "  $0 vanilla   # Launch vanilla Emacs"
    echo "  $0 current   # Load in current session"
    echo "  $0 fix       # Fix transient in current Emacs"
    echo ""
}

launch_doom_sandbox() {
    echo "🏖️ Launching Doom Sandbox..."
    
    # Check if sandbox exists, create if not
    if [ ! -d "$HOME/.config/doom-sandbox" ]; then
        echo "Creating Doom sandbox..."
        "$SCRIPT_DIR/setup-test-profile.sh" doom
    fi
    
    # Check if doom command exists
    if ! command -v doom &> /dev/null; then
        echo "❌ Doom command not found. Using direct Emacs launch..."
        export DOOMDIR="$HOME/.config/doom-test"
        emacs "$@"
    else
        # Sync packages if needed
        export DOOMDIR="$HOME/.config/doom-test"
        echo "📦 Syncing Doom packages..."
        doom sync
        
        # Launch Doom
        echo "🚀 Launching Doom with test profile..."
        emacs "$@"
    fi
}

launch_vanilla() {
    echo "🧪 Launching Vanilla Emacs Profile..."
    
    # Create temp directory for vanilla profile
    VANILLA_DIR="$HOME/.emacs-script-testing"
    mkdir -p "$VANILLA_DIR"
    
    echo "📁 Profile directory: $VANILLA_DIR"
    echo "🎯 Press C-c t for test menu"
    echo ""
    
    # Create init file with fix-transient loaded
    cat > "$VANILLA_DIR/init.el" << EOF
;; Vanilla test profile with transient fixes
(setq user-emacs-directory "$VANILLA_DIR/")

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install transient if needed
(unless (package-installed-p 'transient)
  (package-refresh-contents)
  (package-install 'transient))

;; Load fixes
(load-file "$SCRIPT_DIR/fix-transient.el")
(load-file "$SCRIPT_DIR/simple-loader.el")

;; Keybindings
(global-set-key (kbd "C-c t") 'simple-loader-menu)
(global-set-key (kbd "C-c h") 'simple-loader-load-hybrid)
(global-set-key (kbd "C-c b") 'simple-loader-load-basic)

(message "🧪 Vanilla test profile ready - Press C-c t")
EOF
    
    # Launch with vanilla profile
    emacs --init-directory="$VANILLA_DIR" "$@"
}

load_in_current() {
    echo "📥 Loading in current Emacs session..."
    echo ""
    echo "Execute this in your Emacs:"
    echo ""
    echo "# Fix transient issues first:"
    echo "(load-file \"$SCRIPT_DIR/fix-transient.el\")"
    echo "(fix-doom-transient)"
    echo ""
    echo "# Load simple loader:"
    echo "(load-file \"$SCRIPT_DIR/simple-loader.el\")"
    echo "(simple-loader-menu)"
    echo ""
    echo "# Or load specific implementations:"
    echo "(simple-loader-load-hybrid)     ; Best UX"
    echo "(simple-loader-load-basic)      ; Test framework"
    echo ""
}

fix_transient_current() {
    echo "🔧 Fixing transient in current session..."
    echo ""
    echo "Execute this in your current Emacs:"
    echo ""
    echo "# For Doom Emacs:"
    echo "(load-file \"$SCRIPT_DIR/fix-transient.el\")"
    echo "(fix-doom-transient)"
    echo ""
    echo "# For regular Emacs:"
    echo "(load-file \"$SCRIPT_DIR/fix-transient.el\")"
    echo "(ensure-transient-loaded)"
    echo ""
    echo "# Then test:"
    echo "(load-file \"$SCRIPT_DIR/simple-loader.el\")"
    echo "(simple-loader-menu)"
    echo ""
}

quick_test() {
    echo "🚀 Quick Test Mode"
    echo ""
    echo "Available quick tests:"
    echo "  $SCRIPT_DIR/quick-test.sh basic    # Test basic framework"
    echo "  $SCRIPT_DIR/quick-test.sh hybrid   # Test hybrid implementation"
    echo "  $SCRIPT_DIR/quick-test.sh linus    # Test Linus implementation"
    echo "  $SCRIPT_DIR/quick-test.sh stallman # Test Stallman implementation"
    echo "  $SCRIPT_DIR/quick-test.sh magit    # Test Magit implementation"
    echo ""
    read -p "Which test? [basic/hybrid/linus/stallman/magit]: " choice
    "$SCRIPT_DIR/quick-test.sh" "${choice:-basic}"
}

case "${1:-help}" in
    doom)
        launch_doom_sandbox "${@:2}"
        ;;
    vanilla)
        launch_vanilla "${@:2}"
        ;;
    current)
        load_in_current
        ;;
    fix)
        fix_transient_current
        ;;
    test)
        quick_test
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        echo "❌ Unknown profile: $1"
        echo ""
        show_help
        exit 1
        ;;
esac