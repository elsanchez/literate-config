# Literate Configuration for Emacs (Doom) and Zsh

This repository contains my personal **literate config** files using [Org-mode](https://orgmode.org/).

Each configuration is documented and written in `.org` files, making it easier to understand, maintain, and tangle to actual config files.

## 🤖 AI Integration

This configuration includes **claudemacs** integration for AI assistance directly within Emacs:
- 🔗 **Secret Service Integration**: Secure API key management using system keyring
- ⌨️ **Convenient Keybindings**: Access Claude AI via `SPC c` shortcuts
- 🛡️ **Safe & Secure**: No hardcoded API keys in configuration files

## 📁 Contents

- `doom-config.org`: My Doom Emacs configuration (`init.el`, `config.el`, `packages.el`, `custom.el`)
  - ✨ **LSP and direnv enabled** for development
  - 🤖 **claudemacs integration** for AI assistance
  - 🔧 **Format-on-save** with apheleia
- `zsh-config.org`: My Zsh shell configuration (replaces `.zshrc`)
- `scripts.org`: Utility scripts and functions

## 🚀 Usage

### Initial Setup

To tangle and generate the real configuration files:

```bash
git clone git@gitlab.com:elsanchez/config.git ~/org/literate-config
cd ~/org/literate-config
make
doom sync  # Only needed for Doom Emacs config
```

### Reloading Configuration

After making changes to any `.org` file, you have two options:

**Option 1: Manual reload**
```bash
cd ~/org/literate-config
make && doom sync
```

**Option 2: From within Emacs (Recommended)**
Use the built-in function `elsanchez/doom-reload-config` accessible via `SPC r d`. This function will:
1. Tangle all org files (equivalent to `make`)
2. Run `doom sync`
3. Restart the Emacs daemon
4. Open a new GUI frame

This ensures all changes take effect immediately without manual intervention.

### 🤖 AI Integration Usage

Once configured, use these keybindings for AI assistance:
- `SPC c c` - Start claudemacs chat session
- `SPC c r` - Send selected region to Claude
- `SPC c b` - Send entire buffer to Claude  
- `SPC c h` - Show claudemacs help

**Setup**: Store your Anthropic API key using: `secret-tool store --label="Claude API" service anthropic account claude`

## 📚 Documentation

For detailed information about keybindings, commands, and workflows:
- **[KEYBINDINGS_CHEATSHEET.md](./KEYBINDINGS_CHEATSHEET.md)** - Complete keybindings reference
- **[CHEAT_CARD.md](./CHEAT_CARD.md)** - Printable quick reference
- **[CLAUDE.md](./CLAUDE.md)** - Comprehensive configuration guide
- **[QUICK_REFERENCE.md](./QUICK_REFERENCE.md)** - Essential commands

## License

Feel free to reuse and adapt this configuration.
