# Window Management Guide

This guide covers window management strategies, tools, and keybindings in this literate configuration setup.

## Overview

This configuration provides comprehensive window management capabilities across multiple environments:

- **Emacs**: Window splits, layouts, and workspace management
- **Tmux**: Terminal session and pane management  
- **Desktop**: Application switching and workspace management
- **Integration**: Seamless workflow between all environments

## Emacs Window Management

### Basic Window Operations

| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC w /` | Split window right | Create vertical split |
| `SPC w -` | Split window below | Create horizontal split |
| `SPC w c` | Close window | Close current window |
| `SPC w o` | Close other windows | Keep only current window |
| `SPC w w` | Other window | Switch to other window |
| `SPC w h/j/k/l` | Navigate windows | Move between windows (vim-style) |
| `SPC w H/J/K/L` | Move window | Move window in direction |

### Advanced Window Management

| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC w =` | Balance windows | Make all windows same size |
| `SPC w +` | Increase height | Grow window vertically |
| `SPC w -` | Decrease height | Shrink window vertically |
| `SPC w >` | Increase width | Grow window horizontally |
| `SPC w <` | Decrease width | Shrink window horizontally |
| `SPC w m` | Maximize window | Toggle window maximize |
| `SPC w u` | Undo window config | Restore previous layout |
| `SPC w U` | Redo window config | Re-apply window change |

### Interactive Window Resize (Transient Menu)

The configuration includes a powerful **transient menu** for interactive window management accessible via `SPC w r`. This provides a persistent interface for window operations.

#### Activation
- **Keybinding**: `SPC w r`
- **Function**: `elsanchez/window-resize-transient`
- **Type**: Persistent transient menu (stays open until manually closed)

#### Available Operations

##### 🔧 Resize Operations
| Key | Action | Description |
|-----|--------|-------------|
| `h` | Decrease width | Make window narrower (←) |
| `j` | Increase height | Make window taller (↓) |
| `k` | Decrease height | Make window shorter (↑) |
| `l` | Increase width | Make window wider (→) |

##### 📦 Move Window Operations  
| Key | Action | Description |
|-----|--------|-------------|
| `H` | Move far left | Move window to leftmost position |
| `J` | Move very bottom | Move window to bottom position |
| `K` | Move very top | Move window to top position |
| `L` | Move far right | Move window to rightmost position |

##### ✂️ Split & Manage Operations
| Key | Action | Description |
|-----|--------|-------------|
| `s` | Split below | Create horizontal split below |
| `v` | Split right | Create vertical split to the right |
| `d` | Delete window | Close current window |
| `o` | Delete others | Close all other windows |
| `=` | Balance windows | Make all windows equal size |

##### 🧭 Navigate Operations
| Key | Action | Description |
|-----|--------|-------------|
| `w` | Next window | Switch to next window |
| `W` | Previous window | Switch to previous window |

##### 🚪 Exit Operations
| Key | Action | Description |
|-----|--------|-------------|
| `q` | Quit | Exit transient menu |
| `<escape>` | Quit | Exit transient menu |

#### Usage Example

1. **Open the menu**: `SPC w r`
2. **Resize repeatedly**: Press `h`, `h`, `h` to make window narrower multiple times
3. **Switch operations**: Press `j`, `j` to make it taller, then `=` to balance
4. **Exit when done**: Press `q` or `<escape>`

#### Benefits

- **Persistent Interface**: Menu stays open for multiple operations
- **Visual Feedback**: Shows all available operations at once  
- **Vim-like Navigation**: Uses familiar `hjkl` keys for resize
- **No Memorization**: All commands visible in the interface
- **Efficient Workflow**: Perfect for fine-tuning window layouts

### Workspace Management

| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC TAB n` | New workspace | Create new workspace |
| `SPC TAB d` | Delete workspace | Remove current workspace |
| `SPC TAB r` | Rename workspace | Change workspace name |
| `SPC TAB ]` | Next workspace | Switch to next workspace |
| `SPC TAB [` | Previous workspace | Switch to previous workspace |
| `SPC TAB 1-9` | Switch workspace | Go to specific workspace |
| `SPC TAB TAB` | Last workspace | Switch to previous workspace |

### Buffer Management in Windows

| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC b b` | Switch buffer | Select buffer for current window |
| `SPC b B` | Switch buffer other window | Select buffer for other window |
| `SPC b s` | Save buffer | Save current buffer |
| `SPC b S` | Save all buffers | Save all modified buffers |
| `SPC b k` | Kill buffer | Close current buffer |
| `SPC b K` | Kill other buffers | Close all buffers except current |

## Tmux Session Management

### Session Operations

| Command | Action | Description |
|---------|--------|-------------|
| `tms` | New/attach session | Create or attach to tmux session |
| `tmgo <session>` | Switch session | Switch to named session |
| `tmkill <session>` | Kill session | Terminate session |
| `tmux ls` | List sessions | Show all active sessions |

### Tmux Key Bindings (Prefix: `Ctrl-a`)

#### Window Management
| Keybinding | Action | Description |
|------------|--------|-------------|
| `Prefix + c` | New window | Create new window |
| `Prefix + &` | Kill window | Close current window |
| `Prefix + n` | Next window | Switch to next window |
| `Prefix + p` | Previous window | Switch to previous window |
| `Prefix + 0-9` | Select window | Switch to window by number |
| `Prefix + ,` | Rename window | Change window name |

#### Pane Management
| Keybinding | Action | Description |
|------------|--------|-------------|
| `Prefix + %` | Split vertical | Create vertical pane |
| `Prefix + "` | Split horizontal | Create horizontal pane |
| `Prefix + x` | Kill pane | Close current pane |
| `Prefix + o` | Next pane | Switch to next pane |
| `Prefix + ;` | Last pane | Switch to previous pane |
| `Prefix + {` | Move pane left | Swap with previous pane |
| `Prefix + }` | Move pane right | Swap with next pane |

#### Pane Navigation
| Keybinding | Action | Description |
|------------|--------|-------------|
| `Prefix + h/j/k/l` | Navigate panes | Move between panes (vim-style) |
| `Prefix + Alt+h/j/k/l` | Resize panes | Resize current pane |
| `Prefix + z` | Zoom pane | Toggle pane fullscreen |
| `Prefix + !` | Break pane | Move pane to new window |

## Desktop Window Management

### Application Switching

The configuration includes the `focus_or_launch.sh` script for intelligent application management:

```bash
# Focus existing window or launch new instance
focus_or_launch <application_name>

# Examples:
focus_or_launch firefox     # Focus Firefox or launch it
focus_or_launch emacs       # Focus Emacs or launch it
focus_or_launch terminal    # Focus terminal or launch it
```

### Custom Application Shortcuts

These functions are available in the shell:

| Function | Action | Description |
|----------|--------|-------------|
| `e [file]` | Edit in Emacs | Open file in Emacs or focus Emacs |
| `emacs-frame` | New Emacs frame | Create new Emacs frame |
| `emacs-restart` | Restart Emacs daemon | Restart Emacs daemon with options |

## Integrated Workflow Strategies

### Development Workflow

1. **Terminal-First Approach**:
   - Start with `tms project-name` to create/attach to project session
   - Use tmux panes for different aspects (code, tests, logs, documentation)
   - Use `e filename` to open files in Emacs while staying in terminal

2. **Emacs-Centric Approach**:
   - Use Emacs workspaces for different projects
   - Use built-in terminal (vterm) for shell operations
   - Use magit for git operations within Emacs

3. **Hybrid Approach**:
   - Use tmux for long-running processes and background tasks
   - Use Emacs for intensive editing and code navigation
   - Switch between environments as needed with focus_or_launch

### Project Organization Patterns

#### Single Project Focus
```
Emacs Workspace: "Project"
├── Code files (main editing)
├── Documentation 
└── Tests

Tmux Session: "project"
├── Window 1: Development server
├── Window 2: Test runner
└── Window 3: Git operations
```

#### Multi-Project Context
```
Emacs Workspaces:
├── "Frontend" - React/JS files
├── "Backend" - API/server files  
├── "Docs" - Documentation
└── "Config" - Configuration files

Tmux Sessions:
├── "frontend" - Dev server, build tools
├── "backend" - API server, database
└── "monitoring" - Logs, system monitoring
```

## Window Layout Templates

### Emacs Development Layout
```
┌─────────────────┬─────────────────┐
│                 │                 │
│   Main Code     │   Tests/Docs    │
│                 │                 │
├─────────────────┼─────────────────┤
│                 │                 │
│   Terminal      │   File Tree     │
│                 │                 │
└─────────────────┴─────────────────┘
```

### Tmux Development Layout
```
Session: project
┌─────────────────┬─────────────────┐
│                 │                 │
│   Editor        │   File Watcher  │
│   (Emacs/Vim)   │   (Tests/Build) │
│                 │                 │
├─────────────────┴─────────────────┤
│                                   │
│   Development Server/REPL         │
│                                   │
└───────────────────────────────────┘
```

## Troubleshooting

### Common Issues

#### Emacs Window Problems
- **Windows become unbalanced**: Use `SPC w =` to balance all windows
- **Lost window configuration**: Use `SPC w u` to undo last change
- **Can't navigate between windows**: Check if `SPC w w` works, or use `SPC w h/j/k/l`

#### Tmux Session Problems
- **Can't attach to session**: Check `tmux ls` for existing sessions
- **Panes not responding**: Try `Prefix + :` then `respawn-pane`
- **Lost session**: Sessions persist until system reboot or manual termination

#### Application Focus Issues
- **focus_or_launch not working**: Ensure the script is executable and in PATH
- **Emacs not responding**: Try `emacs-restart` to restart the daemon
- **Multiple instances**: Check if daemon mode is enabled in Emacs config

### Recovery Commands

```bash
# Reset tmux completely
tmux kill-server

# Restart Emacs daemon
emacs-restart

# Check running applications
ps aux | grep -E "(emacs|tmux)"

# Reload shell configuration
source ~/.zshrc
```

## Performance Tips

### Emacs Optimization
- Use workspaces instead of many buffers in one workspace
- Close unused buffers regularly with `SPC b k`
- Use `SPC b K` to close all buffers except current

### Tmux Optimization
- Don't create too many panes in one window (max 4-6)
- Use windows for different contexts, panes for related tasks
- Kill unused sessions with `tmkill session-name`

### System Integration
- Use focus_or_launch instead of launching multiple instances
- Configure window manager to respect application focus requests
- Use daemon mode for Emacs to reduce startup time

## Advanced Configuration

### Custom Window Rules

The configuration can be extended with custom window management rules:

```bash
# In zsh-config.org, you can add custom focus functions:
focus_browser() { focus_or_launch firefox; }
focus_ide() { focus_or_launch emacs; }
focus_term() { focus_or_launch alacritty; }
```

### Tmux Customization

Tmux configuration can be extended in the scripts.org file:

```bash
# Custom tmux session templates
dev_session() {
    tmux new-session -d -s development
    tmux split-window -h
    tmux split-window -v -t 1
    tmux attach-session -t development
}
```

### Emacs Window Management Hooks

Custom Emacs functions can be added to doom-config.org:

```elisp
;; Custom window management functions
(defun my/setup-development-layout ()
  "Set up a development-focused window layout."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-left)
  (split-window-below))
```

This comprehensive guide covers all aspects of window management in the literate configuration environment. Use it as a reference for efficient workspace organization and navigation.