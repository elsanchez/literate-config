# Work Environment Setup (macOS)

This document explains how to set up the work environment integration for macOS with secure credential management.

## Overview

The configuration uses two approaches for work environment setup:
1. **macOS Keychain** - For secure password/token storage
2. **~/.zsh_work_env** - For environment variables (excluded from git)

## macOS Keychain Setup

Store your work credentials securely in macOS Keychain:

```bash
# Work credentials for general use
security add-generic-password -s work-password -a $(whoami) -w "your-work-password"

# Jira credentials
security add-generic-password -s jira-url -a work -w "https://yourcompany.atlassian.net"
security add-generic-password -s jira-email -a work -w "your-email@company.com"
security add-generic-password -s jira-token -a work -w "your-jira-api-token"

# Confluence credentials
security add-generic-password -s confluence-url -a work -w "https://yourcompany.atlassian.net"
security add-generic-password -s confluence-token -a work -w "your-confluence-token"
```

## ~/.zsh_work_env Template

Create `~/.zsh_work_env` with your work-specific environment variables:

```bash
# Work Environment Variables (DO NOT COMMIT TO GIT)
# This file is sourced automatically on macOS

# Go Proxy Configuration
export GOPROXY="https://your-company-proxy.com"
export GOSUMDB="your-sumdb-config"

# Work Credentials
export WORK_USERNAME="your-username"

# Password retrieved securely from Keychain
export WORK_PASSWORD="$(security find-generic-password -w -s work-password -a $(whoami) 2>/dev/null)"

# Additional work-specific environment variables
# export CORPORATE_VPN_CONFIG="path/to/config"
# export WORK_GITLAB_TOKEN="$(security find-generic-password -w -s gitlab-token -a work 2>/dev/null)"
# export WORK_AWS_PROFILE="your-profile"

# Work-specific aliases
alias work-vpn="sudo openvpn /path/to/work.ovpn"
alias work-ssh="ssh -i ~/.ssh/work_key"

# Custom functions for work
work-connect() {
  echo "🔐 Connecting to work environment..."
  # Add your work connection logic here
}
```

## Setup Instructions

1. **Create the work environment file:**
   ```bash
   touch ~/.zsh_work_env
   chmod 600 ~/.zsh_work_env  # Secure permissions
   ```

2. **Add to .gitignore** (if you have a personal dotfiles repo):
   ```bash
   echo ".zsh_work_env" >> ~/.gitignore
   ```

3. **Store credentials in Keychain:**
   ```bash
   # Follow the Keychain setup commands above
   ```

4. **Test the setup:**
   ```bash
   source ~/.zshrc
   echo $WORK_USERNAME
   echo $WORK_PASSWORD  # Should show your password if stored correctly
   ```

## Jira/Confluence Integration

With credentials stored in Keychain, the following features become available in Emacs:

### Keybindings (SPC j):
- `SPC j i` - Get Jira issues
- `SPC j p` - Get Jira projects
- `SPC j c` - Create Jira issue
- `SPC j u` - Update Jira issue
- `SPC j b` - Browse Jira issue
- `SPC j e` - Export to Confluence
- `SPC j r` - REST client template
- `SPC j d` - Open jira directory

### Org-Jira Working Directory:
- Location: `~/org/jira/`
- Automatically created when first used
- Stores Jira issues as org-mode files

## Security Notes

- **Never commit** `.zsh_work_env` to version control
- Use **macOS Keychain** for all sensitive credentials
- Set **restrictive permissions** on `.zsh_work_env`: `chmod 600`
- The configuration **automatically loads** only on macOS
- **Error handling** included - won't break if credentials are missing

## Troubleshooting

### Check if credentials are stored:
```bash
security find-generic-password -s jira-url -a work
security find-generic-password -s work-password -a $(whoami)
```

### Test work environment loading:
```bash
# Reload zsh configuration
source ~/.zshrc

# Check if work variables are loaded
env | grep WORK
env | grep GOPROXY
```

### Emacs integration check:
In Emacs, check if Jira/Confluence packages loaded:
- `M-x describe-variable RET jiralib2-url`
- `SPC j` should show available keybindings

## File Locations

- **Main config**: `~/org/literate-config/zsh-config.org`
- **Work environment**: `~/.zsh_work_env` (private)
- **Jira directory**: `~/org/jira/` (auto-created)
- **Keychain**: macOS Keychain Access app

This setup provides a secure, professional work environment integration while keeping sensitive information properly protected.