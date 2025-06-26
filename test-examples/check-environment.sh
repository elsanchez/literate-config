#!/bin/bash
# Check test environment status and health

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEST_PROFILE_DIR="$HOME/.config/emacs-test-profile"
DOOM_TEST_DIR="$HOME/.config/doom-test"
DOOM_SANDBOX_DIR="$HOME/.config/doom-sandbox"

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

log "🔍 Checking Script Menu Test Environment"
echo ""

# Check required files
echo "📄 Core Files:"
required_files=(
    "fix-transient.el"
    "simple-loader.el" 
    "doom-sandbox-config.el"
    "TEST_CASES.md"
)

for file in "${required_files[@]}"; do
    if [ -f "$SCRIPT_DIR/$file" ]; then
        success "$file"
    else
        error "$file (missing)"
    fi
done

echo ""

# Check bash scripts
echo "🔧 Bash Scripts:"
bash_scripts=(
    "setup-test-profile.sh"
    "launch-profiles.sh"
    "create-doom-sandbox.sh"
    "sync-doom-sandbox.sh"
    "launch-doom-sandbox.sh"
    "launch-emacs-test.sh"
    "launch-doom-test.sh"
    "quick-test.sh"
    "check-environment.sh"
)

for script in "${bash_scripts[@]}"; do
    if [ -f "$SCRIPT_DIR/$script" ]; then
        if [ -x "$SCRIPT_DIR/$script" ]; then
            success "$script (executable)"
        else
            warning "$script (not executable)"
        fi
    else
        error "$script (missing)"
    fi
done

echo ""

# Check test profiles
echo "📁 Test Profiles:"
if [ -d "$TEST_PROFILE_DIR" ]; then
    success "Emacs test profile: $TEST_PROFILE_DIR"
    if [ -f "$TEST_PROFILE_DIR/init.el" ]; then
        success "  init.el present"
    else
        warning "  init.el missing"
    fi
else
    warning "Emacs test profile not created: $TEST_PROFILE_DIR"
fi

if [ -d "$DOOM_TEST_DIR" ]; then
    success "Doom test profile: $DOOM_TEST_DIR"
    for file in "init.el" "config.el" "packages.el"; do
        if [ -f "$DOOM_TEST_DIR/$file" ]; then
            success "  $file present"
        else
            warning "  $file missing"
        fi
    done
else
    warning "Doom test profile not created: $DOOM_TEST_DIR"
fi

if [ -d "$DOOM_SANDBOX_DIR" ]; then
    success "Doom sandbox: $DOOM_SANDBOX_DIR"
else
    warning "Doom sandbox not created: $DOOM_SANDBOX_DIR"
fi

echo ""

# Check external dependencies
echo "🔗 External Dependencies:"
if command -v emacs &> /dev/null; then
    success "Emacs: $(emacs --version | head -n1)"
else
    error "Emacs not found"
fi

if command -v doom &> /dev/null; then
    success "Doom: $(doom --version 2>/dev/null || echo 'installed')"
else
    warning "Doom command not found (optional)"
fi

if command -v python3 &> /dev/null; then
    success "Python3: $(python3 --version)"
else
    warning "Python3 not found (optional for Python runner)"
fi

echo ""

# Health score
total_checks=20
passed_checks=0

# Count successful checks (simplified)
if [ -f "$SCRIPT_DIR/fix-transient.el" ]; then ((passed_checks++)); fi
if [ -f "$SCRIPT_DIR/simple-loader.el" ]; then ((passed_checks++)); fi
if [ -f "$SCRIPT_DIR/doom-sandbox-config.el" ]; then ((passed_checks++)); fi
if [ -f "$SCRIPT_DIR/TEST_CASES.md" ]; then ((passed_checks++)); fi

for script in "${bash_scripts[@]}"; do
    if [ -f "$SCRIPT_DIR/$script" ] && [ -x "$SCRIPT_DIR/$script" ]; then
        ((passed_checks++))
    fi
done

if [ -d "$TEST_PROFILE_DIR" ] && [ -f "$TEST_PROFILE_DIR/init.el" ]; then ((passed_checks++)); fi
if [ -d "$DOOM_TEST_DIR" ]; then ((passed_checks++)); fi
if command -v emacs &> /dev/null; then ((passed_checks++)); fi

health_percentage=$((passed_checks * 100 / total_checks))

echo "🏥 Environment Health: ${health_percentage}% (${passed_checks}/${total_checks})"

if [ $health_percentage -ge 90 ]; then
    success "Environment is ready for testing! 🚀"
elif [ $health_percentage -ge 70 ]; then
    warning "Environment is mostly ready, some optional components missing"
else
    error "Environment needs attention before testing"
fi

echo ""
echo "💡 Quick fixes:"
echo "   ./setup-test-profile.sh    # Create/recreate test profiles"
echo "   ./create-doom-sandbox.sh   # Create Doom sandbox"
echo "   chmod +x *.sh              # Fix script permissions"