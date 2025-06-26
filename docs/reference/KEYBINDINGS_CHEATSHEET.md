# 🎯 Literate Config - Keybindings & Commands Cheatsheet

## 📋 **Doom Emacs Keybindings**

### **🔄 Configuration Reload & Testing**
| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC r d` | `elsanchez/doom-reload-config` | **Interactive config reload** (test/stage/direct/cancel) |
| `SPC r t t` | `elsanchez/test-doom-config` | Test config in isolated environment |
| `SPC r t s` | `elsanchez/stage-doom-config` | Stage config for manual testing |
| `SPC r t r` | `elsanchez/rollback-doom-config` | Rollback to previous configuration |
| `SPC r t l` | `elsanchez/list-config-backups` | List available configuration backups |
| `SPC r t d` | `elsanchez/doom-reload-direct` | Direct reload without testing options |

### **🔧 Emacs Daemon Management**
| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC r e r` | `elsanchez/restart-emacs-daemon` | **Smart restart daemon** (interactive options) |
| `SPC r e a` | `elsanchez/auto-restart-daemon` | Auto restart daemon (systemd detection) |
| `SPC r e d` | `elsanchez/restart-daemon-only` | Restart daemon only (no frame) |
| `SPC r e f` | `elsanchez/open-new-frame` | Open new Emacs frame |

### **🤖 Claude AI Integration (Consolidated)**
| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC c c` | `claudemacs-chat` | **Start claudemacs chat session** |
| `SPC c r` | `claudemacs-region` | Send selected region to Claude |
| `SPC c b` | `claudemacs-buffer` | Send entire buffer to Claude |
| `SPC c h` | `claudemacs-help` | Show claudemacs help and documentation |

### **📋 Jira Integration (macOS)**
| Keybinding | Command | Description |
|------------|---------|-------------|
| `SPC j i` | `org-jira-get-issues` | **Get Jira issues** |
| `SPC j p` | `org-jira-get-projects` | Get Jira projects |
| `SPC j c` | `org-jira-create-issue` | Create new Jira issue |
| `SPC j u` | `org-jira-update-issue` | Update current issue |
| `SPC j b` | `org-jira-browse-issue` | Browse issue in browser |
| `SPC j d` | Open jira directory | Browse ~/org/jira/ directory |

---

## 🐚 **Zsh Commands & Functions**

### **📦 Configuration Management**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `config-backup` | Create timestamped configuration backup | `config-backup` |
| `config-restore` | Interactive backup restoration (with fzf) | `config-restore` |
| `config-list-backups` | List available configuration backups | `config-list-backups` |

### **🔗 Dotfiles Management (Enhanced)**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `config-init-dotfiles` | **Initialize dotfiles repo with symlinks** | `config-init-dotfiles` |
| `config-create-symlinks` | Create symbolic links to dotfiles repo | `config-create-symlinks` |
| `config-sync-to-dotfiles` | Sync generated configs to dotfiles repo | `config-sync-to-dotfiles` |
| `config-status` | **Check dotfiles and symlink status** | `config-status` |
| `config-enhanced-reload` | Enhanced reload with dotfiles integration | `config-enhanced-reload` |

### **🧪 Doom Testing & Staging**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `doom-test-config` | **Test config in isolated environment** | `doom-test-config` |
| `doom-stage-config` | Stage config for manual testing | `doom-stage-config` |
| `doom-rollback` | Rollback to previous Doom configuration | `doom-rollback` |
| `doom-list-backups` | List available Doom configuration backups | `doom-list-backups` |

### **🔄 Emacs Daemon Management (Terminal)**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `emacs-restart` | **Smart restart with auto-detection** | `emacs-restart` |
| `emacs-status` | Check daemon status (service + process + client) | `emacs-status` |
| `emacs-frame` | Open new Emacs frame | `emacs-frame` |
| `emacs-kill` | Stop daemon gracefully | `emacs-kill` |

### **✅ Validation Functions**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `validate-zsh-config` | Validate zsh configuration syntax | `validate-zsh-config` |
| `validate-doom-config` | Validate Doom configuration with `doom doctor` | `validate-doom-config` |
| `validate-elisp-syntax` | Validate Emacs Lisp file syntax | `validate-elisp-syntax ~/.config/doom/config.el` |

### **🔧 Dependency Management**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `check-dependencies` | Check for missing system dependencies | `check-dependencies` |
| `install-dependencies` | **Auto-install missing dependencies** | `install-dependencies` |
| `setup-literate-config` | **Complete environment setup** | `setup-literate-config` |

### **📱 Tmux Utilities**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `tms` | **Select tmux session with fzf** | `tms` |
| `tmgo [session]` | Create or attach to tmux session | `tmgo work` or `tmgo` (default) |
| `tmkill` | **Kill tmux session with fzf** | `tmkill` |

### **📁 File & Navigation Utilities**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `mkcd <dir>` | Create directory and cd into it | `mkcd ~/projects/new-project` |
| `up <n>` | Navigate up N directories | `up 3` |
| `e <file>` | Open file with Emacs in terminal | `e config.el` |
| `open <path>` | Open path with Dolphin file manager | `open .` |

### **📹 Media Download**
| Command | Description | Usage Example |
|---------|-------------|---------------|

### **🔧 System Utilities**
| Command | Description | Usage Example |
|---------|-------------|---------------|
| `please <command>` | Alias for sudo | `please pi tmux` |
| `reload!` | Reload zsh configuration | `reload!` |
| `restart-audio` | Restart audio system (pipewire/pulseaudio) | `restart-audio` |

### **📦 Cross-Platform Package Management**
| Alias | Linux (APT) | macOS (Homebrew) | Description |
|-------|-------------|------------------|-------------|
| `pi` | `sudo apt install` | `brew install` | **Install package** |
| `pr` | `sudo apt remove` | `brew uninstall` | Remove package |
| `ps` | `apt search` | `brew search` | Search packages |
| `pu` | `sudo apt update && sudo apt upgrade` | `brew update && brew upgrade` | **Update and upgrade** |
| `pc` | `sudo apt autoremove` | `brew cleanup` | Clean unused packages |
| `pinfo` | `apt show` | `brew info` | Show package information |

> **Note**: The system automatically detects your OS and uses the appropriate package manager.

---

## 🚀 **Interactive Workflows**

### **🧪 Safe Configuration Testing Workflow**
```bash
# 1. Edit doom-config.org
# 2. Test safely:
SPC r d → [t]           # Test in isolation first
# If tests pass:
SPC r d → [d] → [a]     # Direct deployment + auto restart

# From terminal:
doom-test-config        # Test first
emacs-restart          # Smart restart if tests pass
```

### **🎭 Staging Workflow for Manual Testing**
```bash
# 1. Edit doom-config.org  
# 2. Stage for testing:
SPC r d → [s]           # Stage configuration
# Test manually in Emacs
# If satisfied: keep changes
# If not:
doom-rollback          # Rollback quickly
```

### **⚡ Emergency Recovery**
```bash
# If Emacs won't start:
doom-rollback          # Quick rollback
emacs-status          # Check daemon status
emacs-restart         # Restart daemon

# If complete failure:
config-restore        # Restore from git backups
setup-literate-config # Nuclear option: complete reset
```

---

## 🍎 **macOS-Specific Features**

### **🔧 Environment Variables (Auto-configured)**
| Variable | Description | Detection |
|----------|-------------|-----------|
| `JAVA_HOME` | JDK 17 installation | Auto-detected from `/Library/Java/JavaVirtualMachines/` |
| `M3_HOME` | Maven installation path | Dynamic version detection from Homebrew |
| `M3` | Maven bin directory | Based on M3_HOME |
| `TNS_ADMIN` | Oracle TNS configuration | `~/tools/instantclient_19_8/network/admin` |
| `TERM` | Terminal type | Set to `xterm` |

### **🏗️ Architecture Detection**
```bash
# ARM64 (Apple Silicon)
eval "$(/opt/homebrew/bin/brew shellenv)"

# Intel (x86_64)  
eval "$(/usr/local/bin/brew shellenv)"
```

### **📁 PATH Restoration (macOS)**
| Path | Tool/Purpose | Condition |
|------|-------------|-----------|
| `~/.emacs.d/bin` | Doom Emacs | macOS specific |
| `~/.local/bin` | Python local packages | If directory exists |
| `~/.cargo/bin` | Rust tools | If directory exists |
| `/opt/homebrew/opt/openssl@3/bin` | OpenSSL | Homebrew installation |
| `~/bin/sqlcl/bin` | Oracle SQLcl | If directory exists |
| `~/bin` | Personal binaries | If directory exists |
| `$GOPATH/bin` | Go tools | If GOPATH set |
| `~/.local/pipx/venvs/*/bin` | **NEW**: pipx tools | Dynamic detection |

### **📁 PATH Additions (Linux)**
| Path | Tool/Purpose | Condition |
|------|-------------|-----------|
| `~/.config/emacs/bin` | Doom Emacs | Linux specific |
| `~/.local/bin` | Python local packages | If directory exists |
| `~/.local/share/pipx/venvs/*/bin` | **NEW**: pipx tools | Dynamic detection |
| `/opt/nvim-linux-x86_64/bin` | Neovim | If directory exists |

### **🔐 Work Environment (.zsh_work_env)**
```bash
# Secure credential management
export WORK_PASSWORD="$(security find-generic-password -w -s work-password -a $(whoami))"

# Go proxy configuration
export GOPROXY="company-proxy"
export GOSUMDB="company-sumdb"
```

### **🎯 macOS Aliases**
| Alias | Command | Description |
|-------|---------|-------------|
| `pi` | `brew install` | Package install |
| `pr` | `brew uninstall` | Package remove |
| `pu` | `brew update && brew upgrade` | Package update |
| `pc` | `brew cleanup` | Package cleanup |
| `gh` | `history \| grep` | **NEW**: Grep history |

---

## 📊 **Menu Options Reference**

### **SPC r d (Main Reload Menu)**
- **`[t]`** - Test first (safest option)
- **`[s]`** - Stage for testing  
- **`[d]`** - Direct reload
- **`[c]`** - Cancel

### **SPC r e r (Daemon Restart Menu)**
- **`[a]`** - Auto restart (recommended)
- **`[m]`** - Manual restart
- **`[r]`** - Reload only (no restart)
- **`[s]`** - Skip restart

### **emacs-restart-alternative (Terminal Menu)**
- **`1`** - Kill and restart daemon
- **`2`** - Client-based restart
- **`3`** - Manual instructions

---

## 🏗️ **Build Commands**

### **Make Commands**
| Command | Description |
|---------|-------------|
| `make` | **Tangle all .org files** |
| `make scripts` | Tangle only scripts |
| `make clean` | Clean generated files |

### **Doom Commands**
| Command | Description |
|---------|-------------|
| `doom sync` | Sync Doom configuration |
| `doom doctor` | Check configuration health |
| `doom upgrade` | Upgrade Doom Emacs |

---

## 💡 **Pro Tips**

### **🎯 Most Used Commands**
1. **`SPC r d → [t]`** - Always test before deploying
2. **`emacs-restart`** - Smart daemon restart from terminal
3. **`check-dependencies`** - Verify environment health
4. **`config-backup`** - Manual backup before major changes
5. **`tms`** - Quick tmux session switching

### **🛡️ Safety First**
- Always use **`[t]`** (test) option for config changes
- Run **`check-dependencies`** when setting up new environment
- Use **`doom-rollback`** or **`config-restore`** for quick recovery
- Monitor with **`emacs-status`** and **`systemctl --user status emacs.service`**

### **⚡ Speed Tips**
- Use **`setup-literate-config`** for new machine setup
- **`install-dependencies`** handles all missing tools automatically
- **`pu`** for quick system updates
- **`tms`** and **`tmgo`** for efficient tmux workflow

---

*Generated for Literate Configuration System v2.0*  
*For issues or improvements, edit the .org files and run the enhanced reload system*