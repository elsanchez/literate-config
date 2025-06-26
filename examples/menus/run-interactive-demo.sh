#!/bin/bash
# Run Textual Interactive Script Runner Demo

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
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

feature() {
    echo -e "${CYAN}🎯${NC} $1"
}

log "🚀 Textual Interactive Script Runner Demo"
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
    log "Creating interactive demo configuration..."
    python3 "$SCRIPT_DIR/textual_interactive.py" --demo
    success "Interactive demo configuration created"
fi

echo

# Show comprehensive features overview
log "🎮 Textual Interactive Script Runner - Complete Feature Set:"
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
feature "🖥️ EMBEDDED TERMINAL"
echo "   • Real-time script interaction with pseudo-terminal (pty)"
echo "   • Send input to running scripts while they execute"
echo "   • Capture live output streaming"
echo "   • Interrupt processes with Ctrl+C"
echo "   • Support for interactive scripts (marked with 🖥️ icon)"
echo
feature "🔗 CLICKABLE HYPERLINKS"
echo "   • Script paths → Click to open in default editor"
echo "   • Directory paths → Click to open in file manager"
echo "   • Web URLs → Click to open in browser"
echo "   • Config files → Click to edit directly"
echo "   • Rich text formatting with link styles"
echo
feature "📂 FILE SYSTEM INTEGRATION"
echo "   • xdg-open integration for all file types"
echo "   • Directory browser from Interactive tab"
echo "   • Config file quick access"
echo "   • Script directory navigation"
echo "   • Path validation and resolution"
echo
feature "🎯 ADVANCED EXECUTION MODES"
echo "   • 🔍 Preview Only - Show command without execution"
echo "   • 🏃 Background - Run with PID tracking"
echo "   • 🖥️ Interactive Terminal - Full embedded terminal"
echo "   • 📄 Capture Output - Standard output capture"
echo "   • Visual mode selector in configuration screen"
echo
feature "✨ ENHANCED UI COMPONENTS"
echo "   • 📝 Text Input with rich placeholders"
echo "   • 🔘 Select Dropdown with visual feedback"
echo "   • ☑️ Switch/Checkbox with toggle animation"
echo "   • 🔢 Number Input with validation"
echo "   • Scrollable configuration screens"
echo "   • Modal dialogs for help and terminal"
echo
feature "⌨️ KEYBOARD SHORTCUTS"
echo "   • Ctrl+T - Open system terminal"
echo "   • Ctrl+L - Show hyperlinks demo"
echo "   • Ctrl+R - Reload configuration"
echo "   • Ctrl+Q - Quit application"
echo "   • F1 - Interactive help system"
echo "   • Escape - Close modals/go back"
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo

log "🎯 Demo Scripts Available:"
echo
echo "📦 **interactive-deploy** (🖥️ Interactive Mode)"
echo "   • Environment selection dropdown"
echo "   • Interactive mode checkbox"
echo "   • Server name text input"
echo "   • Try: Select 'Interactive Terminal' execution mode"
echo "   • Opens embedded terminal for real-time interaction"
echo
echo "📁 **file-browser**"
echo "   • Directory path input with clickable output"
echo "   • Demonstrates file system integration"
echo "   • Click on paths in output to open directories"
echo
echo "⚡ **quick-command**"
echo "   • No arguments - executes immediately"
echo "   • Shows direct execution flow"
echo "   • Demonstrates simple script handling"
echo

log "🎮 Complete Testing Guide:"
echo
echo "1️⃣ **Basic Navigation:**"
echo "   • Use left tree panel to browse scripts"
echo "   • Click on scripts in main table"
echo "   • Switch between tabs (Scripts/Statistics/Interactive/Logs)"
echo
echo "2️⃣ **Configuration Screen Testing:**"
echo "   • Click 'interactive-deploy' to open config"
echo "   • Try all UI components:"
echo "     - Select dropdown for environment"
echo "     - Toggle the interactive mode switch"
echo "     - Enter text in server name field"
echo "   • Click the script path (📂 Script:) to open file"
echo "   • Try 'Help & Links' button for documentation"
echo
echo "3️⃣ **Execution Mode Testing:**"
echo "   • Select different execution modes:"
echo "     - Preview Only → See command without running"
echo "     - Background → Run with PID tracking"
echo "     - Interactive Terminal → Full embedded terminal"
echo "     - Capture Output → Standard output display"
echo
echo "4️⃣ **Interactive Terminal Demo:**"
echo "   • Select 'Interactive Terminal' mode"
echo "   • Click Execute to open embedded terminal"
echo "   • Type commands in the input field"
echo "   • Press Enter or click 'Send Input'"
echo "   • Try 'Interrupt' for Ctrl+C simulation"
echo "   • Watch real-time output streaming"
echo
echo "5️⃣ **Hyperlinks & File Integration:**"
echo "   • Go to 'Interactive' tab"
echo "   • Try all buttons:"
echo "     - 🖥️ Open System Terminal"
echo "     - 🔗 Show Hyperlinks Demo"
echo "     - 📂 Open Script Directory"
echo "     - ⚙️ Open Config File"
echo "   • Use Ctrl+T and Ctrl+L shortcuts"
echo
echo "6️⃣ **Advanced Features:**"
echo "   • Check Statistics tab for script counts"
echo "   • Monitor Logs tab for all activity"
echo "   • Use F1 for interactive help"
echo "   • Try Ctrl+R to reload configuration"
echo

log "🎪 Interactive Elements to Test:"
echo
echo "🖱️  **Clickable Elements:**"
echo "   • Script paths in configuration screens"
echo "   • File/directory links in output"
echo "   • Hyperlinks in help screens"
echo "   • Navigation tree items"
echo
echo "⌨️  **Keyboard Interactions:**"
echo "   • Enter key in terminal input"
echo "   • Tab navigation between widgets"
echo "   • Escape to close modals"
echo "   • All Ctrl+ shortcuts"
echo
echo "🎮 **Real-time Features:**"
echo "   • Live output in embedded terminal"
echo "   • Process interruption (Ctrl+C)"
echo "   • Background job PID tracking"
echo "   • Config reload without restart"
echo

warning "🔒 Safety Note:"
echo "   All demo scripts run in preview mode (safe)"
echo "   No actual system changes will be made"
echo "   Interactive terminal is sandboxed"
echo

log "🚀 Starting Textual Interactive Script Runner..."
echo "💡 Press Ctrl+Q to quit at any time"
echo "🎯 Try the Interactive Terminal mode for the full experience!"
echo

# Start the application
python3 "$SCRIPT_DIR/textual_interactive.py"