# Literate Configuration for Emacs (Doom) and Zsh

This repository contains my personal **literate config** files using [Org-mode](https://orgmode.org/).

Each configuration is documented and written in `.org` files, making it easier to understand, maintain, and tangle to actual config files.

## 📁 Contents

- `doom-config.org`: My Doom Emacs configuration (`init.el`, `config.el`, `packages.el`, `custom.el`).
- `zsh-config.org`: My Zsh shell configuration (replaces `.zshrc`).

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

## License

Feel free to reuse and adapt this configuration.
