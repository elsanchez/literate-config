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
    echo "  doom     - Doom Emacs sandbox (recommended)"
    echo "  vanilla  - Vanilla Emacs profile"
    echo "  current  - Load in current Emacs session"
    echo ""
    echo "Examples:"
    echo "  $0 doom      # Launch Doom sandbox"
    echo "  $0 vanilla   # Launch vanilla Emacs"
    echo "  $0 current   # Load in current session"
    echo ""
}

launch_doom_sandbox() {
    echo "🏖️ Launching Doom Sandbox..."
    
    # Check if sandbox exists, create if not
    if [ ! -d "$HOME/.config/doom-sandbox" ]; then
        echo "Creating Doom sandbox..."
        "$SCRIPT_DIR/create-doom-sandbox.sh"
    fi
    
    # Launch sandbox
    "$SCRIPT_DIR/launch-doom-sandbox.sh"
}

launch_vanilla() {
    echo "🧪 Launching Vanilla Emacs Profile..."
    
    # Create temp directory for vanilla profile
    VANILLA_DIR="$HOME/.emacs-script-testing"
    mkdir -p "$VANILLA_DIR"
    
    echo "📁 Profile directory: $VANILLA_DIR"
    echo "🎯 Press C-c h for hybrid implementation"
    echo ""
    
    # Launch with vanilla profile
    emacs --init-directory="$VANILLA_DIR" \
          --load="$SCRIPT_DIR/vanilla-profile.el"
}

load_in_current() {
    echo "📥 Loading in current Emacs session..."
    echo ""
    echo "Execute this in your Emacs:"
    echo ""
    echo "(progn"
    echo "  (load-file \"$SCRIPT_DIR/test-runner.el\")"
    echo "  (load-file \"$SCRIPT_DIR/hybrid-implementation.el\")"
    echo "  (test-runner-setup-test-environment)"
    echo "  (hybrid-scripts))"
    echo ""
}

case "${1:-help}" in
    doom)
        launch_doom_sandbox
        ;;
    vanilla)
        launch_vanilla
        ;;
    current)
        load_in_current
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