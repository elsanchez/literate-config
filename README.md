# Literate Configuration for Emacs (Doom) and Zsh

This repository contains my personal **literate config** files using [Org-mode](https://orgmode.org/).

Each configuration is documented and written in `.org` files, making it easier to understand, maintain, and tangle to actual config files.

## 📁 Contents

- `doom-config.org`: My Doom Emacs configuration (`init.el`, `config.el`, `packages.el`, `custom.el`).
- `zsh-config.org`: My Zsh shell configuration (replaces `.zshrc`).

## 🚀 Usage

To tangle and generate the real configuration files:

```bash
git clone git@gitlab.com:elsanchez/config.git ~/org/literate-config
cd ~/org/literate-config
make
```

## License

Feel free to reuse and adapt this configuration.
