# ⚡ Quick Reference - Literate Config

## 🔥 **Most Used Commands**

### **Emacs (Essential)**
```
SPC r d → [t]     # Test config safely
SPC r d → [s]     # Stage for testing  
SPC r d → [d]     # Direct deployment
SPC r e r → [a]   # Auto restart daemon
```

### **Terminal (Essential)**
```bash
doom-test-config     # Test before deploy
emacs-restart        # Smart daemon restart
check-dependencies   # Verify environment
config-backup        # Manual backup
yaml-param-runner.py config.yaml -s SCRIPT  # Execute scripts with validation
```

### **Emergency Recovery**
```bash
doom-rollback        # Quick Doom rollback
config-restore       # Git backup restore
emacs-status         # Check daemon health
```

---

## 📋 **Complete Keybinding Summary**

### **Doom Emacs Keybindings**
| Key | Action |
|-----|--------|
| `SPC r d` | Interactive config reload |
| `SPC r t t` | Test config isolated |
| `SPC r t s` | Stage for testing |
| `SPC r t r` | Rollback config |
| `SPC r t l` | List backups |
| `SPC r e r` | Smart daemon restart |
| `SPC r e a` | Auto restart |
| `SPC r e f` | New frame |
| `SPC a c` | Claude chat |
| `SPC a r` | Claude region |

### **Essential Zsh Commands**
| Command | Purpose |
|---------|---------|
| `doom-test-config` | Test safely |
| `emacs-restart` | Smart restart |
| `config-backup` | Create backup |
| `config-restore` | Restore backup |
| `check-dependencies` | Check deps |
| `install-dependencies` | Install deps |
| `tms` | Tmux sessions |
| `pu` | Update system (cross-platform) |
| `reload!` | Reload zsh |

---

## 🛡️ **Safety Workflow**
1. Edit `.org` files
2. `SPC r d → [t]` (test first)
3. If OK: `SPC r d → [d] → [a]` (deploy + restart)
4. If issues: `doom-rollback`

## ⚡ **Speed Workflow**  
1. `doom-test-config && emacs-restart`
2. Or: `SPC r d → [t]` then proceed if tests pass

*Keep this handy for daily development!*