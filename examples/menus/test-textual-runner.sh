#!/bin/bash
# Test script for Textual Script Runner

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

log "🎨 Testing Textual Script Runner"
echo

# Check if virtual environment is activated
if [[ "$VIRTUAL_ENV" == "" ]]; then
    warning "Virtual environment not detected"
    echo "Activating virtual environment..."
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

# Create demo configuration
log "Creating demo configuration..."
python3 "$SCRIPT_DIR/textual_script_runner.py" --demo
success "Demo configuration created"

echo

# Show features
log "🎮 Textual Script Runner Features:"
echo
echo "🎨 **Modern TUI Interface:**"
echo "   - Responsive layout with Header/Footer"
echo "   - Tabbed interface (Scripts, Statistics, Logs)"
echo "   - Tree navigation for menus"
echo "   - Data table for script overview"
echo
echo "🎯 **Command Palette (Ctrl+P):**"
echo "   - VS Code-style command search"
echo "   - Search scripts by name and description"
echo "   - Filter by tags"
echo "   - Quick navigation commands"
echo
echo "🧩 **Advanced UI Components:**"
echo "   - **Radio Buttons**: Environment selection (development/staging/production)"
echo "   - **Checkboxes**: Enable/disable features (backup, force operations)"
echo "   - **Multi-Select**: Choose multiple items (features, databases, services)"
echo "   - **Sliders**: Numeric values with ranges (parallel jobs, compression level)"
echo "   - **Select Dropdowns**: Single choice from list (notification methods)"
echo "   - **Text Input**: Free-form text with validation"
echo "   - **Number Input**: Numeric input with min/max constraints"
echo
echo "🚀 **Script Execution:**"
echo "   - Modal dialog for argument collection"
echo "   - Command preview before execution"
echo "   - Real-time output display"
echo "   - Async and sync execution modes"
echo
echo "⌨️  **Keyboard Shortcuts:**"
echo "   - **Ctrl+P**: Command Palette"
echo "   - **Ctrl+R**: Reload Configuration"
echo "   - **Ctrl+Q**: Quit Application"
echo "   - **F1**: Help"
echo "   - **Escape**: Close modals"
echo "   - **Enter**: Execute/Confirm"

echo
log "🎯 Demo Scripts Available:"
echo
echo "1. **advanced-deploy**: Comprehensive deployment"
echo "   - Radio: Environment selection"
echo "   - Multi-select: Features to enable"
echo "   - Checkbox: Backup option"
echo "   - Slider: Parallel jobs (1-16)"
echo "   - Number: Timeout setting"
echo "   - Select: Notification method"
echo
echo "2. **database-operations**: Database management"
echo "   - Radio: Operation type (backup/restore/optimize/migrate)"
echo "   - Multi-select: Target databases"
echo "   - Slider: Compression level (0-9)"
echo
echo "3. **service-control**: System service management"
echo "   - Multi-select: Services to manage"
echo "   - Radio: Action (start/stop/restart/status/enable/disable)"
echo "   - Checkbox: Force operation"
echo "   - Slider: Wait time between operations"

echo
log "📁 Configuration Location:"
echo "   ~/.config/textual-script-runner.yaml"

echo
log "🎮 How to Test All Components:"
echo
echo "1. **Start the application:**"
echo "   python3 $SCRIPT_DIR/textual_script_runner.py"
echo
echo "2. **Try Command Palette:**"
echo "   - Press Ctrl+P"
echo "   - Type 'deploy' to find deployment script"
echo "   - Type 'tag:database' to filter by tag"
echo "   - Try system commands like 'settings' or 'reload'"
echo
echo "3. **Test UI Components:**"
echo "   - Click on 'advanced-deploy' in the table"
echo "   - Use each UI component type"
echo "   - Try the Preview Command button"
echo "   - Execute to see real-time output"
echo
echo "4. **Navigation:**"
echo "   - Use the tree on the left"
echo "   - Switch between tabs (Scripts/Statistics/Logs)"
echo "   - Try different keyboard shortcuts"

echo
log "🚀 Starting Textual Script Runner..."
echo "💡 Press Ctrl+P for Command Palette"
echo "🔄 Press Ctrl+R to reload config"
echo "❌ Press Ctrl+Q to quit"
echo

# Start the application
python3 "$SCRIPT_DIR/textual_script_runner.py"