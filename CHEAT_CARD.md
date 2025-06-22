# 🎯 Literate Config - Cheat Card

```
┌─────────────────────────────────────────────────────────────────┐
│                    DOOM EMACS KEYBINDINGS                      │
├─────────────────────────────────────────────────────────────────┤
│ SPC r d         │ Interactive config reload (t/s/d/c)          │
│ SPC r t t       │ Test config in isolation                     │
│ SPC r t s       │ Stage config for testing                     │
│ SPC r t r       │ Rollback to previous config                  │
│ SPC r e r       │ Smart daemon restart (a/m/r/s)              │
│ SPC r e a       │ Auto restart daemon                          │
│ SPC r e f       │ Open new frame                               │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                      ZSH COMMANDS                               │
├─────────────────────────────────────────────────────────────────┤
│ doom-test-config       │ Test config safely                    │
│ emacs-restart          │ Smart daemon restart                  │
│ config-backup          │ Create configuration backup           │
│ config-restore         │ Restore from backup (interactive)     │
│ doom-rollback          │ Quick Doom config rollback            │
│ check-dependencies     │ Check missing dependencies            │
│ install-dependencies   │ Auto-install missing tools            │
│ setup-literate-config  │ Complete environment setup            │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                     TMUX & UTILITIES                           │
├─────────────────────────────────────────────────────────────────┤
│ tms                    │ Select tmux session (fzf)            │
│ tmgo [session]         │ Create/attach tmux session           │
│ tmkill                 │ Kill tmux session (fzf)              │
│ mkcd <dir>             │ Create dir and cd into it             │
│ up <n>                 │ Go up N directories                   │
│ e <file>               │ Edit with Emacs (terminal)           │
│ apu                    │ apt update && upgrade                 │
│ reload!                │ Reload zsh config                    │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                    INTERACTIVE MENUS                           │
├─────────────────────────────────────────────────────────────────┤
│ SPC r d menu:          │ [t]est [s]tage [d]irect [c]ancel     │
│ SPC r e r menu:        │ [a]uto [m]anual [r]eload [s]kip      │
│ emacs-restart menu:    │ 1)kill&start 2)client 3)manual      │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                      WORKFLOWS                                 │
├─────────────────────────────────────────────────────────────────┤
│ SAFE TESTING:          │ SPC r d → [t] → (if OK) → [d] → [a]  │
│ STAGING:               │ SPC r d → [s] → test → doom-rollback │
│ EMERGENCY:             │ doom-rollback / config-restore       │
│ NEW SETUP:             │ setup-literate-config                │
└─────────────────────────────────────────────────────────────────┘

                          Print & Keep Handy! 🖨️
```