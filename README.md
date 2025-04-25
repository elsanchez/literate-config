# Literate Configuration for Emacs (Doom) and Zsh

This repository contains my personal **literate config** files using [Org-mode](https://orgmode.org/).  
Each configuration is documented and written in `.org` files, making it easier to understand, maintain, and tangle to actual config files.

## Contents

- `doom-config.org`: My Doom Emacs configuration (includes `init.el`, `config.el`, `packages.el`, and more).
- `zsh-config.org`: My Zsh shell configuration (replaces `.zshrc`).
  
## Usage

To generate the real config files from the `.org` sources:

1. Open the `.org` file in Emacs.
2. Run `M-x org-babel-tangle` or `C-c C-v t`.

Make sure Emacs is set up to evaluate Org Babel blocks.

## Cloning

```bash
git clone git@gitlab.com:youruser/literate-config.git ~/org/literate-config
```

## License

Feel free to reuse and adapt this configuration.