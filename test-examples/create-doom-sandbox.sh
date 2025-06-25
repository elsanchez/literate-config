#!/bin/bash
# Create Doom Emacs sandbox for script menu testing

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SANDBOX_DIR="$HOME/.config/doom-sandbox"
DOOM_DIR="$HOME/.config/doom"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() {
    echo -e "${BLUE}[$(date +'%H:%M:%S')]${NC} $1"
}

success() {
    echo -e "${GREEN}✓${NC} $1"
}

warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

log "🏖️ Creating Doom Sandbox for Script Menu Testing"

# Check if Doom is installed
if [ ! -d "$DOOM_DIR" ]; then
    warning "Doom Emacs not found at $DOOM_DIR"
    echo "Please install Doom Emacs first: https://github.com/doomemacs/doomemacs"
    exit 1
fi

# Create sandbox directory
log "Creating sandbox directory: $SANDBOX_DIR"
mkdir -p "$SANDBOX_DIR"

# Copy Doom configuration as base
log "Setting up Doom configuration..."
cp -r "$DOOM_DIR"/* "$SANDBOX_DIR/" 2>/dev/null || true

# Create sandbox-specific init.el
cat > "$SANDBOX_DIR/init.el" << 'EOF'
;;; init.el -*- lexical-binding: t; -*-

;; Doom sandbox for script menu testing

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home rojw

       :completion
       (corfu +orderless)  ; complete with cap(f), cape and a flying feather!
       vertico           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       treemacs          ; a project drawer, like NERDTree for vim
       unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       snippets          ; my elves. They type so I don't have to

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       magit             ; a git porcelain for Emacs
       lookup            ; navigate your code and its documentation

       :lang
       emacs-lisp        ; drown in parentheses
       markdown          ; writing docs for people to ignore
       org               ; organize your plain life in plain text
       sh                ; she sells {ba,z,fi}sh shells on the C xor

       :config
       (default +bindings +smartparens))
EOF

# Create sandbox-specific config.el
cat > "$SANDBOX_DIR/config.el" << EOF
;;; config.el -*- lexical-binding: t; -*-

;; Load sandbox configuration
(load-file "$SCRIPT_DIR/doom-sandbox-config.el")
EOF

# Create packages.el for required packages
cat > "$SANDBOX_DIR/packages.el" << 'EOF'
;; -*- no-byte-compile: t; -*-
;;; packages.el

;; Required packages for script menu testing
(package! transient)
EOF

# Create launcher script
cat > "$SCRIPT_DIR/launch-doom-sandbox.sh" << EOF
#!/bin/bash
# Launch Doom Sandbox for Script Menu Testing

echo "🏖️ Launching Doom Sandbox..."
echo "📁 Config: $SANDBOX_DIR"
echo "🎯 Press SPC t t for main test menu"
echo ""

# Set environment variables
export DOOMDIR="$SANDBOX_DIR"
export DOOMPROFILE="sandbox"

# Launch Emacs with sandbox config
emacs --init-directory="$SANDBOX_DIR" "\$@"
EOF

chmod +x "$SCRIPT_DIR/launch-doom-sandbox.sh"

# Create quick sync script
cat > "$SCRIPT_DIR/sync-doom-sandbox.sh" << EOF
#!/bin/bash
# Sync Doom sandbox packages

export DOOMDIR="$SANDBOX_DIR"
doom sync
EOF

chmod +x "$SCRIPT_DIR/sync-doom-sandbox.sh"

success "Doom sandbox created successfully!"
echo ""
echo "🚀 Quick start:"
echo "   ./launch-doom-sandbox.sh    # Launch sandbox"
echo "   ./sync-doom-sandbox.sh      # Sync packages (if needed)"
echo ""
echo "📁 Sandbox location: $SANDBOX_DIR"
echo "🎯 In Emacs: Press SPC t t for test menu"
echo "🔧 To customize: Edit $SANDBOX_DIR/config.el"