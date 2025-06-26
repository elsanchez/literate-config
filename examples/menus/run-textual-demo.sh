#!/bin/bash
# Run Textual Demo Script Runner

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

log "🎨 Textual Script Runner Demo"
echo

# Check if virtual environment is activated
if [[ "$VIRTUAL_ENV" == "" ]]; then
    log "Activating virtual environment..."
    source "$SCRIPT_DIR/venv/bin/activate"
fi

# Check dependencies
log "Checking dependencies..."

if python3 -c "import textual" 2>/dev/null; then
    success "Textual available: $(python3 -c "import textual; print(textual.__version__)")"
else
    error "Textual not found. Installing..."
    uv pip install textual
fi

if python3 -c "import yaml" 2>/dev/null; then
    success "PyYAML available"
else
    error "PyYAML not found. Installing..."
    uv pip install pyyaml
fi

echo

# Create demo configuration if it doesn't exist
if [ ! -f "$HOME/.config/textual-script-runner.yaml" ]; then
    log "Creating demo configuration..."
    python3 "$SCRIPT_DIR/textual_demo.py" --demo
    success "Demo configuration created"
fi

echo

# Show features
log "🎮 Textual Script Runner Features:"
echo
echo "✅ **Working UI Components:**"
echo "   📝 Text Input - Free form text with defaults"
echo "   🔘 Select Dropdown - Choose from predefined options"
echo "   ☑️ Checkbox/Switch - On/off toggles"
echo "   🔢 Number Input - Numeric values with validation"
echo
echo "🎯 **Demo Scripts Available:**"
echo "   • **deploy-with-options**: Deployment with all component types"
echo "   • **backup-with-compression**: Backup with compression options"
echo "   • **simple-script**: Script without arguments (direct execution)"
echo
echo "🧭 **Navigation:**"
echo "   • Left panel: Tree navigation with menus and scripts"
echo "   • Right panel: Tabbed interface (Scripts/Statistics/Logs)"
echo "   • Click on scripts to configure and execute"
echo "   • Scripts with arguments open configuration screen"
echo "   • Scripts without arguments execute immediately"
echo
echo "⌨️  **Keyboard Shortcuts:**"
echo "   • Ctrl+R - Reload Configuration"
echo "   • Ctrl+Q - Quit Application"
echo "   • F1 - Show Help"
echo "   • Escape - Go back (in config screens)"
echo
echo "🔍 **What to Try:**"
echo "   1. Click on 'deploy-with-options' in Scripts tab"
echo "   2. Configure each UI component:"
echo "      - Select environment from dropdown"
echo "      - Toggle backup checkbox"
echo "      - Enter numeric values"
echo "      - Enter server name"
echo "   3. Click Preview to see generated command"
echo "   4. Click Execute to run (safe demo mode)"
echo "   5. Watch real-time output in execution screen"

echo
log "🚀 Starting Textual Script Runner Demo..."
echo "💡 All scripts run in demo mode (100% safe)"
echo "🎯 Try all the UI components to see how they work!"
echo

# Start the application
python3 "$SCRIPT_DIR/textual_demo.py"