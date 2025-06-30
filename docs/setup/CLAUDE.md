# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a literate configuration repository that uses Org-mode to document and generate configuration files for:
- **Doom Emacs** (`doom-config.org` → `~/.config/doom/`)
- **Zsh shell** (`zsh-config.org` → `~/.zshrc`)
- **Tmux** (`tmux-config.org` → `~/.config/tmux/tmux.conf.local` or `~/.tmux.conf.local`)
- **Custom scripts** (`scripts.org` → `~/.local/bin/`)

The repository follows a literate programming approach where configuration is documented in `.org` files and "tangled" to generate actual config files.

## AI Integration

The configuration includes **claudemacs** integration for AI assistance directly within Emacs:
- Uses `cpoile/claudemacs` package for Claude AI integration  
- Secret Service integration for secure API key management
- **Consolidated keybindings** under `SPC c` prefix (claude-code conflicts resolved)
- **Cross-platform compatibility**: All Claude-related functions include existence checks
- **v4.0 Update**: Removed conflicting claude-code configuration for clean operation

## Jira Integration (macOS)

**v4.2 STREAMLINED**: Simplified Jira-only workflow with secure credential management:
- **macOS Keychain** authentication for secure credential storage
- **org-jira** for issue management and project tracking
- **REST client** for general API interactions
- **Keybindings** under `SPC j` prefix for complete workflow
- **Working directory**: `~/org/jira/` (auto-created)
- **v4.2 UPDATE**: Confluence integration removed for simplicity

## Essential Commands

### Building/Tangling Configurations
```bash
# Tangle all .org files to generate configs
make

# Tangle with automatic backup (recommended)
make all-safe

# Tangle only scripts
make scripts

# Clean generated files
make clean
```

### Backup Management (Makefile)
```bash
# Create backup of current configurations
make backup

# List available backups
make list-backups

# Show restore instructions
make restore-backup

# Clean old backups (keep last 10)
make clean-backups

# Get help on all available targets
make help
```

### Configuration Management (Enhanced)
```bash
# Check system dependencies
check-dependencies

# Install missing dependencies automatically
install-dependencies

# Complete setup for new environment (with dotfiles option)
setup-literate-config

# Create configuration backup
config-backup

# List available backups
config-list-backups

# Restore from backup (interactive)
config-restore

# Validate configurations
validate-zsh-config
validate-doom-config
```

### Dotfiles Management (New!)
```bash
# Initialize dotfiles repository with symlinks
config-init-dotfiles

# Create symbolic links to dotfiles repo
config-create-symlinks

# Sync generated configs to dotfiles repo
config-sync-to-dotfiles

# Check dotfiles and symlink status
config-status

# Enhanced reload with dotfiles integration
config-enhanced-reload
```

### Doom Emacs Testing & Staging
```bash
# Test configuration in isolated environment
doom-test-config

# Stage configuration for manual testing
doom-stage-config

# Rollback to previous configuration
doom-rollback

# List available Doom backups
doom-list-backups

# Validate Emacs Lisp syntax
validate-elisp-syntax <file>
```

### Emacs Daemon Management
```bash
# Smart restart with auto-detection
emacs-restart

# Check daemon status
emacs-status

# Open new frame
emacs-frame

# Stop daemon gracefully
emacs-kill
```


### After Making Changes
```bash
# From Makefile (recommended for safety)
make all-safe && doom sync   # Backup + tangle + sync

# Enhanced reload with testing options
# In Emacs: SPC r d (interactive menu: test, stage, direct, cancel)

# Testing-specific commands in Emacs:
# SPC r t t - Test config in isolation
# SPC r t s - Stage config for testing  
# SPC r t r - Rollback to previous config
# SPC r t l - List available backups
# SPC r t d - Direct reload (no testing)

# Daemon management in Emacs:
# SPC r e r - Smart restart daemon (with options)
# SPC r e a - Auto restart daemon
# SPC r e d - Restart daemon only
# SPC r e f - Open new frame

# Manual reload (traditional)
make && doom sync

# v4.2 UPDATE: All syntax errors resolved, magit working correctly
```

### Initial Setup
```bash
git clone <repo-url> ~/org/literate-config
cd ~/org/literate-config
setup-literate-config  # This will install dependencies and setup everything
```

## Architecture

### File Structure
- `*.org` files contain the source configuration with documentation
- `Makefile` handles tangling (converting org to config files)
- Each `.org` file uses `#+BEGIN_SRC` blocks with `:tangle` headers to specify output paths
- `~/.config-backups/` contains git-versioned configuration backups
- `~/.dotfiles/` contains symlinked configuration repository (optional enhanced mode)

### Key Components
- **doom-config.org**: Contains `init.el`, `config.el`, `packages.el`, and `custom.el` sections
  - LSP and direnv enabled for development
  - claudemacs for AI integration with conditional loading (compatible with macOS/systems without Claude)
  - Cross-platform doom binary path detection (`~/.emacs.d/bin/doom` on macOS, `~/.config/emacs/bin/doom` on Linux)
  - Graceful fallback to alternative Claude packages when available
  - Format-on-save enabled with apheleia
- **zsh-config.org**: **Cross-platform** Zsh setup with automatic OS detection
  - **Package Management**: Automatic brew (macOS) vs apt (Linux) detection
  - **Aliases**: Universal package commands (`pi`, `pr`, `ps`, `pu`, `pc`, `pinfo`)
  - **File Managers**: Native `open` (macOS) vs `xdg-open` (Linux)
  - **Daemon Management**: systemd with manual fallbacks
  - Oh My Zsh, powerlevel10k, and essential utilities
- **tmux-config.org**: Unified cross-platform tmux configuration with oh-my-tmux integration
  - Dynamic path resolution based on OS detection
  - Automatic clipboard integration (xclip on Linux, pbcopy on macOS)
  - Platform-specific plugin paths and status scripts
  - Single configuration source that adapts to the platform
- **scripts.org**: Utility scripts like `focus_or_launch.sh` for window management

### Enhanced Features

#### Backup System
- **Makefile Integration**: Automatic backups via `make all-safe` or `make backup`
- **Emacs Integration**: Backup functions accessible via `SPC r d`
- Git-based versioning with timestamped commits
- Easy restoration with `make list-backups` and `make restore-backup`
- Backup cleanup with `make clean-backups`

#### Dotfiles Management (Enhanced)
- Optional symlink-based dotfiles repository at `~/.dotfiles/`
- Proper git versioning of all configuration files
- Automatic syncing between generated configs and dotfiles repo
- Clean separation between source (.org) and target (symlinked) files

#### Validation & Error Handling
- Pre-deployment syntax validation for zsh
- Doom configuration validation with `doom doctor`
- Dependency checking and automated installation
- Robust error handling with helpful messages

#### Dependency Management
- Automatic detection of missing tools and plugins
- One-command installation of all dependencies
- Support for Oh My Zsh plugins, system packages, and Python tools

### Custom Functions

#### Emacs Functions
- `elsanchez/doom-reload-config`: Enhanced config reload with testing options, backup, validation, and error handling
- `elsanchez/test-doom-config`: Test configuration in isolated environment
- `elsanchez/stage-doom-config`: Stage configuration for manual testing
- `elsanchez/rollback-doom-config`: Rollback to previous configuration  
- `elsanchez/doom-reload-direct`: Direct reload without testing options
- `elsanchez/backup-configs`: Create timestamped configuration backups
- `elsanchez/validate-doom-config`: Validate Doom configuration before applying
- `elsanchez/list-config-backups`: Interactive backup browser

#### Zsh Functions
- Configuration management: `config-backup`, `config-restore`, `config-list-backups`
- Validation: `validate-zsh-config`, `validate-doom-config`, `validate-elisp-syntax`
- Testing & staging: `doom-test-config`, `doom-stage-config`, `doom-rollback`, `doom-list-backups`
- Daemon management: `emacs-restart`, `emacs-status`, `emacs-frame`, `emacs-kill`
- Dotfiles management: `config-init-dotfiles`, `config-create-symlinks`, `config-sync-to-dotfiles`, `config-status`, `config-enhanced-reload`
- Dependency management: `check-dependencies`, `install-dependencies`, `setup-literate-config`
- Tmux utilities: `tms()`, `tmgo()`, `tmkill()` (with dependency checking)
- File utilities: `mkcd()`, `up()`, `e()` (cross-platform file operations)

#### YAML Parameter Runner Functions
- Script execution: Execute bash scripts with YAML-configured parameters
- Parameter validation: Comprehensive validation for text, number, select, and boolean types
- Interactive collection: Prompt users for parameter values with defaults and help text
- Configuration management: YAML-based script configuration with examples and validation rules

### Error Recovery
If configuration deployment fails:
1. Previous configuration is automatically backed up
2. Error messages provide specific guidance
3. Use `config-list-backups` and `config-restore` to rollback
4. Validation functions help identify issues before deployment

## Development Workflow

### Safe Testing Approach (Recommended)
1. Edit `.org` files directly (not the generated configs)
2. Test changes safely:
   - **Option A**: `SPC r d` → `[t]` - Test in isolated environment first
   - **Option B**: `SPC r d` → `[s]` - Stage for manual testing
   - **Option C**: Use `doom-test-config` from terminal
3. If tests pass, proceed with deployment
4. Commit `.org` file changes (not generated files)

### Direct Deployment (Advanced)
1. Edit `.org` files directly
2. `SPC r d` → `[d]` for direct deployment with backup and validation
3. Enhanced reload function will:
   - Create automatic backup
   - Validate configurations
   - Apply changes only if validation passes
   - Provide rollback options on failure
4. Test configurations
5. Commit `.org` file changes

### Dotfiles-Based Workflow (Enhanced)
1. Set up dotfiles system: `config-init-dotfiles`
2. Edit `.org` files in literate-config repository
3. Generate configs: `make all-safe`
4. Sync to dotfiles: `config-sync-to-dotfiles`
5. Configurations are automatically symlinked to `~/.dotfiles/`
6. All changes are git-versioned in dotfiles repository
7. Use `config-status` to monitor symlink health

### Testing Methods Available

#### 1. Isolated Testing (`doom-test-config`)
- Creates temporary test environment in `~/.doom-test`
- Tangles configs to test directory
- Validates Emacs Lisp syntax
- Tests `doom sync` without affecting live config
- Cleans up automatically after testing

#### 2. Staging Environment (`doom-stage-config`)  
- Backs up current config to timestamped directory
- Applies new config to live environment
- Allows manual testing of actual Emacs session
- Easy rollback with `doom-rollback`

#### 3. Syntax Validation (`validate-elisp-syntax`)
- Checks Emacs Lisp syntax using batch mode
- Validates parentheses and basic structure
- Can be used on individual files

## Cross-Platform Compatibility

### macOS Enhanced Support (v4.0)
Complete macOS integration with professional development environment:

#### **Environment Variables** (Auto-configured):
- **JAVA_HOME**: Auto-detected JDK 17 installation
- **M3_HOME/M3**: Dynamic Maven version detection from Homebrew
- **TNS_ADMIN**: Oracle TNS configuration path
- **TERM**: Set to xterm for compatibility

#### **PATH Restoration** (All missing paths restored):
- `~/.emacs.d/bin` - Doom Emacs (macOS-specific)
- `~/.local/bin` - Python local packages
- `~/.cargo/bin` - Rust/Cargo tools
- `/opt/homebrew/opt/openssl@3/bin` - Updated OpenSSL
- `~/bin/sqlcl/bin` - Oracle SQLcl
- `~/bin` - Personal binaries
- `$GOPATH/bin` - Go tools

#### **Architecture Detection**:
- **ARM64** (Apple Silicon): `/opt/homebrew/bin/brew shellenv`
- **Intel** (x86_64): `/usr/local/bin/brew shellenv`

#### **Work Environment Integration**:
- **~/.zsh_work_env**: Secure credential file (excluded from git)
- **macOS Keychain**: Secure password/token storage
- **Conditional loading**: Only on macOS, fails gracefully

### Cross-Platform Claude Integration
The configuration is designed to work seamlessly across different platforms, including macOS systems where Claude packages may not be available:

- **Conditional Loading**: All Claude-related packages (`claudemacs`, `claude-code`, `emacs-claude-code`) use existence checks
- **Function Validation**: Keybindings and function calls are wrapped with `fboundp` and `boundp` checks
- **Graceful Degradation**: Systems without Claude packages will load all other functionality normally
- **No Breaking Errors**: The `init.el` generation will succeed even without AI packages installed

### Validated Functions
The following functions include existence checks for cross-platform compatibility:
- `claudemacs-chat`, `claudemacs-region`, `claudemacs-buffer`, `claudemacs-help`
- `claude-chat`, `claude-code-region`, `claude-code-buffer`, `claude-code-file`, `claude-code-fix-region`
- `ecc-auto-periodical-toggle`, `--ecc-vterm-utils-enable-yank-advice`
- Variables: `--ecc-auto-response-responses`, `ecc-auto-periodical-commands`

## Troubleshooting

### Common Issues
- **Missing dependencies**: Run `check-dependencies` then `install-dependencies`
- **Powerlevel10k not loading**: The config now auto-detects and provides installation instructions
- **Zsh plugin errors**: Enhanced config checks for plugin availability before loading
- **Configuration corruption**: Use `config-restore` to rollback to working version
- **Claude packages not available (macOS)**: All Claude-related functions include existence checks and fail gracefully
- **init.el generation failures**: Fixed with conditional loading of all AI-related packages and functions

### Recovery Commands
```bash
# Check what's missing
check-dependencies

# Restore last working configuration
config-restore

# Rollback Doom config specifically
doom-rollback

# Test current config without deploying
doom-test-config

# Validate current setup
validate-zsh-config && validate-doom-config

# Complete environment reset
setup-literate-config
```

## Quick Reference

### Interactive Testing Workflow
```bash
# From Emacs
SPC r d → [t]     # Test first (safest)
SPC r d → [s]     # Stage for testing
SPC r d → [d]     # Direct deployment
SPC r d → [c]     # Cancel

# From terminal
doom-test-config   # Test in isolation
doom-stage-config  # Stage for testing
doom-rollback      # Rollback if needed
```

### AI Integration (claudemacs)
```bash
# From Emacs (consolidated under SPC c)
SPC c c          # Start Claude chat
SPC c r          # Ask Claude about region
SPC c b          # Ask Claude about buffer
SPC c h          # Claude help

# Note: v4.0 - Conflicts with claude-code resolved
# Clean loading on all systems without errors
```

### Jira Workflow (macOS)
```bash
# From Emacs (SPC j prefix)
SPC j i          # Get Jira issues
SPC j p          # Get Jira projects
SPC j c          # Create Jira issue
SPC j u          # Update current issue
SPC j b          # Browse issue in browser
SPC j d          # Open jira directory

# Setup credentials in Keychain first
# See docs/reference/WORK_ENVIRONMENT.md for setup
```

### Emergency Recovery
```bash
# If Emacs won't start after config change
doom-rollback

# If you need to find backups
doom-list-backups
config-list-backups

# Nuclear option - restore from git backups
config-restore
```

## 🚨 Common Errors and Solutions

### End-of-File Errors in config.el

**Error**: `Debugger entered--Lisp error: (doom-user-error "doom/config.el" (end-of-file "/home/user/.config/doom/config.el"))`

**Common Causes**:
- **Nested functions**: Functions defined inside other functions (incorrect Emacs Lisp syntax)
- **Unbalanced parentheses**: Missing or extra closing parentheses
- **Syntax errors**: Malformed s-expressions

**Solution Pattern**:
```elisp
;; ❌ INCORRECT: Nested function definition
(defun outer-function ()
  "Documentation"
  (interactive)
  ;; ... function body ...
  
  (defun inner-function ()  ; ← This is WRONG
    "This will cause end-of-file errors"
    ;; ... body ...))

;; ✅ CORRECT: Independent function definitions
(defun outer-function ()
  "Documentation"
  (interactive)
  ;; ... function body ...
  )  ; ← Properly closed

(defun inner-function ()  ; ← Separate function
  "This is correct"
  ;; ... body ...
  )  ; ← Properly closed
```

**Debugging Steps**:
1. Check for nested `defun` statements
2. Verify parentheses balance: `M-x check-parens` in Emacs
3. Use `make tangle` to regenerate config.el
4. Test syntax: `emacs --batch -l ~/.config/doom/config.el`

**Prevention**:
- Keep all function definitions at top level
- Use proper indentation to visualize nesting
- Regularly validate syntax during development

### Function Loading Issues

**Error**: Functions defined in `zsh-config.org` not available in shell

**Solution**: Functions need to be tangled and shell reloaded
```bash
make && source ~/.zshrc
# OR restart terminal
```

**Error**: Doom functions not working after configuration changes

**Solution**: Use the built-in reload system
```
SPC r d  # In Emacs: Interactive reload with testing options
```

## 📚 Reference Documentation

This repository includes comprehensive reference materials:

- **`docs/reference/KEYBINDINGS_CHEATSHEET.md`** - Complete keybindings and commands reference
- **`docs/reference/QUICK_REFERENCE.md`** - Essential commands for daily use  
- **`docs/reference/CHEAT_CARD.md`** - Printable cheat card format
- **`docs/setup/CLAUDE.md`** - This comprehensive guide
- **✨ NEW `docs/reference/WORK_ENVIRONMENT.md`** - macOS work environment setup with Keychain integration

### Key Maps and Documentation Guidelines
- Siempre que se agregen, modifiquen o eliminen los key maps documentalo en el archivo docs/reference/KEYBINDINGS_CHEATSHEET.md y docs/reference/CHEAT_CARD.md
- **Recuerda generar un commmit message y preguntar si se quiere publicar siempre tras finalizar los cambios**

For quick lookup during development, refer to these files or keep the cheat card handy.

### Project-Specific Memories

- ✅ **Multi-repository push configured**: `git push` now syncs to both GitHub and GitLab automatically
- Individual remotes still available: `git push origin` (both repos) or `git push gitlab` (GitLab only)