#!/bin/bash
# Test script for Interactive Script Runner

set -euo pipefail

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

log "🐍 Testing Interactive Script Runner"
echo

# Check Python
if ! command -v python3 &> /dev/null; then
    error "Python 3 not found. Please install Python 3"
    exit 1
fi

success "Python 3 found: $(python3 --version)"

# Check if Rich is available
if python3 -c "import rich" 2>/dev/null; then
    success "Rich library available"
else
    warning "Rich library not found"
    echo "Installing Rich library..."
    if pip3 install rich --user; then
        success "Rich library installed"
    else
        error "Failed to install Rich library"
        echo "You can still use the script but with limited UI"
    fi
fi

echo

# Create demo configuration
log "Creating demo configuration..."
python3 "$SCRIPT_DIR/interactive_script_runner.py" --demo
success "Demo configuration created"

echo

# Show available demos
log "🎮 Available Demo Options:"
echo
echo "1. Full Interactive Demo:"
echo "   python3 $SCRIPT_DIR/interactive_script_runner.py"
echo
echo "2. With Custom Config:"
echo "   python3 $SCRIPT_DIR/interactive_script_runner.py --config /path/to/config.yaml"
echo
echo "3. Direct Demo Execution:"
echo "   # Try the webapp deployment demo"
echo "   $SCRIPT_DIR/demo-scripts/deploy.sh --environment production --features auth,monitoring --backup --parallel_jobs 8 --notification_channels slack"
echo
echo "   # Try the database backup demo"  
echo "   $SCRIPT_DIR/demo-scripts/backup.sh --databases users,products,orders --compression gzip --verify_backup --retention_days 60"
echo
echo "   # Try the service management demo"
echo "   $SCRIPT_DIR/demo-scripts/service.sh --services nginx,mysql --action restart --force"

echo
log "🎯 Interactive Components Demo:"
echo
echo "The interactive runner includes these components:"
echo
echo "📝 Text Input:"
echo "   - Simple text fields with validation"
echo "   - File path selection with extension filtering"
echo "   - Directory path selection"
echo
echo "🔘 Radio Buttons:"
echo "   - Single selection from multiple options"
echo "   - Visual indicators (● selected, ○ unselected)"
echo
echo "☑️  Checkboxes:"
echo "   - Boolean on/off toggles"
echo "   - Multiple independent selections"
echo
echo "📋 Multi-Select Lists:"
echo "   - Select multiple items from a list"
echo "   - Toggle items on/off with numbers"
echo "   - Commands: number to toggle, 'done' to finish, 'clear' to clear all"
echo
echo "🔢 Number Input:"
echo "   - Integer input with min/max validation"
echo "   - Automatic range checking"
echo
echo "📂 File/Directory Selection:"
echo "   - File picker with extension filtering"
echo "   - Directory picker with validation"
echo "   - Path expansion and existence checking"

echo
log "🚀 Starting Interactive Demo..."
echo "Press Ctrl+C to exit at any time"
echo

# Start the interactive runner
python3 "$SCRIPT_DIR/interactive_script_runner.py"