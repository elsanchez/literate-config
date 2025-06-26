#!/bin/bash
# Launch Doom Sandbox for Script Menu Testing

set -euo pipefail

SANDBOX_DIR="$HOME/.config/doom-sandbox"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
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

error() {
    echo -e "${RED}✗${NC} $1"
}

log "🏖️ Launching Doom Sandbox..."
log "📁 Config: $SANDBOX_DIR"
log "🎯 Press SPC t t for main test menu"
echo ""

# Check if sandbox exists
if [ ! -d "$SANDBOX_DIR" ]; then
    warning "Sandbox not found. Creating it now..."
    "$SCRIPT_DIR/create-doom-sandbox.sh"
fi

# Check if packages are synced
if [ ! -d "$SANDBOX_DIR/.local/straight" ] && [ ! -d "$SANDBOX_DIR/elpa" ]; then
    warning "Packages not synced. Running sync..."
    "$SCRIPT_DIR/sync-doom-sandbox.sh"
fi

# Set environment variables
export DOOMDIR="$SANDBOX_DIR"
export DOOMPROFILE="sandbox"

# Check if doom command exists
if command -v doom &> /dev/null; then
    log "Using Doom launcher..."
    # Launch with Doom
    doom run "$@"
else
    log "Doom command not found. Using direct Emacs launch..."
    # Launch Emacs directly with sandbox config
    emacs --init-directory="$SANDBOX_DIR" "$@"
fi

success "Doom sandbox session ended"
