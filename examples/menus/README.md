# 🚀 Script Menu Examples

Colección completa de ejemplos de interfaces para ejecutar scripts, desde Emacs Lisp hasta Python TUI moderno con **Textual**.

## 📁 Estructura de Ejemplos

```
examples/menus/
├── 📝 Emacs Implementations
│   ├── stallman.el           # Richard Stallman style (comprehensive)
│   ├── linustorv.el         # Linus Torvalds style (pragmatic)
│   └── magit-enhanced.el    # Magit style (visual)
│
├── 🐍 Python Implementations  
│   ├── script_runner.py           # Rich TUI version
│   ├── interactive_script_runner.py  # Enhanced Rich with components
│   └── textual_script_runner.py      # Modern Textual with Command Palette
│
├── 🧪 Testing & Demos
│   ├── test-interactive-runner.sh    # Rich version tester
│   ├── test-textual-runner.sh       # Textual version tester
│   └── demo-scripts/                # Sample scripts for testing
│
└── venv/                    # Python virtual environment
```

## 🎯 Comparación de Implementaciones

| Feature | Stallman.el | Linus.el | Magit.el | Rich Python | **Textual Python** |
|---------|-------------|----------|----------|-------------|----------------|
| **Philosophy** | Comprehensive | Fast & Direct | Visual | Interactive | **Modern TUI** |
| **UI Style** | Documentation | Minimal | Git-like | Rich Text | **Full App** |
| **Components** | Help System | Quick Menu | Tree View | Widgets | **Advanced Widgets** |
| **Customization** | Extensive | Minimal | Moderate | Config File | **YAML + UI** |
| **Learning Curve** | High | Low | Medium | Medium | **Medium-High** |
| **Best For** | Power Users | Quick Tasks | Git Users | CLI Apps | **Desktop-like** |

## 🎨 UI Components Demo

### 📝 Text Input
```yaml
- name: server_name
  type: text
  description: Server hostname
  default: "localhost"
```

### 🔘 Radio Buttons (Single Selection)
```yaml
- name: environment
  type: radio
  description: Target environment
  choices: [development, staging, production]
  default: development
```

### ☑️ Checkboxes (Boolean Toggle)
```yaml
- name: backup_enabled
  type: checkbox
  description: Create backup before deployment
  default: true
```

### 📋 Multi-Select Lists
```yaml
- name: features
  type: multi_select
  description: Features to enable
  choices: [auth, analytics, caching, cdn, monitoring]
  default: [auth, monitoring]
```

### 🎚️ Sliders (Numeric Range)
```yaml
- name: parallel_jobs
  type: slider
  description: Number of parallel jobs
  min_value: 1
  max_value: 16
  step: 1
  default: 4
```

### 🔢 Number Input
```yaml
- name: timeout
  type: number
  description: Timeout in seconds
  min_value: 30
  max_value: 3600
  default: 300
```

### 📂 File/Directory Selection
```yaml
- name: config_file
  type: file
  description: Configuration file
  file_extensions: [.yaml, .yml, .json]
  required: false
```

### 📋 Select Dropdown
```yaml
- name: notification_method
  type: select
  description: Notification method
  choices: [email, slack, discord, webhook, none]
  default: email
```

## 🚀 Quick Start

### 1. **Rich Python Version** (Colorful CLI)
```bash
cd examples/menus
source venv/bin/activate
./test-interactive-runner.sh
```

**Features:**
- Rich text interface with colors
- Interactive prompts
- All UI components
- Command preview
- Real-time execution

### 2. **Textual Python Version** (Modern TUI) ⭐ **RECOMENDADO**
```bash
cd examples/menus
source venv/bin/activate
./test-textual-runner.sh
```

**Features:**
- Full-screen TUI application
- **VS Code-style command palette** (Ctrl+P) 🎯
- **Tabbed interface** (Scripts, Statistics, Logs)
- **Tree navigation** with menus and submenus
- **Modal dialogs** for script configuration
- **Real-time logs** and execution output
- **Advanced widgets**: sliders, radio buttons, checkboxes, multi-select

### 3. **Emacs Versions** (For Emacs users)
```bash
cd test-examples
./launch-profiles.sh doom    # Doom Emacs sandbox
./launch-profiles.sh vanilla # Vanilla Emacs
```

## 🎮 Interactive Components Testing

### **Command Palette Demo** (Textual only) ⭐
1. Start Textual version: `./test-textual-runner.sh`
2. Press `Ctrl+P` to open command palette
3. Try these searches:
   - `deploy` - Find deployment scripts
   - `tag:database` - Filter by database tag
   - `service` - Find service management
   - `reload` - System commands
   - `quit` - Exit application

### **All UI Components Demo**
1. Run Textual version
2. Select "advanced-deploy" script from table
3. Experience each component:
   - **Radio**: Choose environment (development/staging/production)
   - **Multi-select**: Toggle features with numbers, type 'done' to finish
   - **Checkbox**: Enable/disable backup (toggle switch)
   - **Slider**: Adjust parallel jobs (1-16) with visual feedback
   - **Number**: Set timeout value (30-3600 seconds)
   - **File**: Enter file path (validates extensions)
   - **Select**: Pick notification method from dropdown

### **Real-time Execution**
1. Configure a script with all options
2. Click "Preview Command" to see generated bash command
3. Execute to see real-time output in modal window
4. All scripts are in demo mode (100% safe to run)

## 🛠️ Creating Custom Scripts

### **YAML Configuration Example:**
```yaml
menus:
  - name: "My Scripts"
    description: "Custom script collection"
    scripts:
      - name: "my-deployment"
        path: "./scripts/deploy.sh"
        description: "Deploy my application"
        category: "Deployment"
        args:
          - name: target
            type: radio
            description: Target server
            choices: [dev, staging, prod]
            default: dev
          - name: features
            type: multi_select
            description: Features to enable
            choices: [ssl, cdn, monitoring]
          - name: dry_run
            type: checkbox
            description: Dry run mode
            default: true
          - name: workers
            type: slider
            description: Number of workers
            min_value: 1
            max_value: 10
            step: 1
            default: 4
        tags: [deployment, production]
        help_text: "Deploys application with selected features"
        preview_mode: true  # Safe testing mode
```

### **Script Argument Parsing:**
```bash
#!/bin/bash
# Your script: deploy.sh

while [[ $# -gt 0 ]]; do
    case $1 in
        --target)
            TARGET="$2"
            shift 2
            ;;
        --features)
            FEATURES+=("$2")
            shift 2
            ;;
        --dry_run)
            DRY_RUN=true
            shift
            ;;
        --workers)
            WORKERS="$2"
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            shift
            ;;
    esac
done

echo "🚀 Deploying to: $TARGET"
echo "📦 Features: ${FEATURES[*]}"
echo "🔍 Dry run: $DRY_RUN"
echo "👥 Workers: $WORKERS"
```

## 🎯 Best Practices

### **For UI Design:**
- Use **radio buttons** for mutually exclusive choices
- Use **checkboxes** for boolean flags
- Use **multi-select** for independent options  
- Use **sliders** for numeric ranges with visual feedback
- Use **select** for single choice from many options
- Use **text input** for free-form data
- Use **file/directory** for path selection

### **For Script Integration:**
- Always provide sensible defaults
- Add help text for complex options
- Use `preview_mode: true` during development
- Group related options logically
- Validate inputs in your scripts
- Handle errors gracefully

### **For User Experience:**
- Keep argument names short but descriptive
- Provide clear descriptions and help text
- Use consistent naming conventions
- Test with real use cases
- Document expected behavior
- Use categories to organize scripts

## 🔧 Development

### **Environment Setup:**
```bash
# Create virtual environment
cd examples/menus
python3 -m venv venv
source venv/bin/activate

# Install dependencies with uv (faster)
uv pip install rich pyyaml textual

# Or with pip
pip install rich pyyaml textual
```

### **Testing:**
```bash
# Test Rich version
python3 interactive_script_runner.py --demo
python3 interactive_script_runner.py

# Test Textual version (recommended)
python3 textual_script_runner.py --demo
python3 textual_script_runner.py

# Test with custom config
python3 textual_script_runner.py --config my-config.yaml
```

### **Keyboard Shortcuts (Textual):**
- **Ctrl+P**: Command Palette (like VS Code)
- **Ctrl+R**: Reload Configuration
- **Ctrl+Q**: Quit Application  
- **F1**: Show Help
- **Escape**: Close modals/Cancel
- **Enter**: Execute/Confirm
- **Tab**: Navigate between widgets

## 🎨 Screenshots & Demo

### **Textual Command Palette:**
```
┌─ Command Palette ─────────────────────────────────────┐
│ > deploy                                              │
├───────────────────────────────────────────────────────┤
│ 🚀 Run advanced-deploy - Advanced deployment with... │
│ 🚀 Run database-operations - Database backup and...  │
│ 🏷️ Tag: deployment                                   │
│ 🏠 Go to main menu                                   │
└───────────────────────────────────────────────────────┘
```

### **Script Configuration Modal:**
```
┌─ Configure: advanced-deploy ──────────────────────────┐
│ Advanced deployment with comprehensive options        │
│                                                       │
│ ┌─ Environment ────────────────────────────────────┐ │
│ │ ● development  ○ staging  ○ production          │ │
│ └─────────────────────────────────────────────────── │
│                                                       │
│ ┌─ Features (multi-select) ────────────────────────┐ │  
│ │ ☑ auth  ☑ monitoring  ☐ caching  ☐ cdn        │ │
│ └─────────────────────────────────────────────────── │
│                                                       │
│ ┌─ Parallel Jobs ──────────────────────────────────┐ │
│ │ ████████░░░░░░░░ 4                              │ │
│ └─────────────────────────────────────────────────── │
│                                                       │
│ [ Cancel ]  [ Preview Command ]  [ Execute ]         │
└───────────────────────────────────────────────────────┘
```

## 📚 Further Reading

- **Rich Documentation**: https://rich.readthedocs.io/
- **Textual Documentation**: https://textual.textualize.io/
- **PyYAML Documentation**: https://pyyaml.org/
- **Command Palette Pattern**: Inspired by VS Code Command Palette

---

🎉 **Ready to create amazing script interfaces!** 

**Start with the Textual version** para la experiencia más moderna, o usa Rich para herramientas de línea de comandos más simples.

**Command Palette** es el feature estrella - presiona `Ctrl+P` y busca cualquier script o comando! 🎯