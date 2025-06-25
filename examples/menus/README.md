# Script Menu Examples

This directory contains different implementations of script management interfaces, each with their own philosophy and approach.

## Files

### `linustorv.el` - Linus Torvalds Style
- **Philosophy**: Pragmatic, no-nonsense approach
- **Features**: Direct script execution, minimal configuration
- **Usage**: `M-x linus-scripts`
- **Target**: Developers who want things to just work

```emacs-lisp
(load-file "examples/menus/linustorv.el")
(global-set-key (kbd "C-c x") 'linus-scripts)
```

### `stallman.el` - Richard Stallman Style  
- **Philosophy**: Freedom-focused, comprehensive documentation
- **Features**: Extensive help, user freedom emphasis, GPL compliance
- **Usage**: `M-x stallman-scripts`
- **Target**: Users who value software freedom and comprehensive features

```emacs-lisp
(load-file "examples/menus/stallman.el")
(global-set-key (kbd "C-c s") 'stallman-scripts)
```

### `magit-enhanced.el` - Advanced Magit-Style Interface
- **Philosophy**: Visual exploration, dynamic discovery
- **Features**: 
  - Auto-discovery of scripts with metadata parsing
  - Dynamic menu generation
  - Template-based script creation
  - Tag-based organization
- **Usage**: `M-x magit-enhanced-scripts`
- **Target**: Power users who want visual script management

```emacs-lisp
(load-file "examples/menus/magit-enhanced.el")
(global-set-key (kbd "C-c M-s") 'magit-enhanced-scripts)
```

### `script_runner.py` - Python TUI Alternative
- **Philosophy**: Cross-platform, configuration-driven
- **Features**:
  - YAML-based configuration
  - Rich TUI interface (when available)
  - Auto-discovery of scripts
  - Argument collection with validation
  - Visual menu tree exploration

```bash
# Install dependencies
pip install rich pyyaml

# Run directly
./script_runner.py

# Or discover scripts
./script_runner.py --discover
```

## Script Metadata Format

All implementations support extracting metadata from script comments:

```bash
#!/bin/bash
# Description: Deploy application to target environment
# Tags: deployment, ci-cd, production
# Help: This script deploys the application using Docker
#       Supports multiple environments and rollback
# @arg environment: Target environment (dev/staging/prod)
# @arg version: Version to deploy (optional)

# Your script content here
```

## Configuration Examples

### Python Runner Configuration (`~/.config/script-runner.yaml`)

```yaml
menus:
  - name: "Development"
    description: "Development scripts"
    scripts:
      - name: "deploy"
        path: "~/scripts/deploy.sh"
        description: "Deploy application"
        args:
          - name: "environment"
            description: "Target environment"
            choices: ["dev", "staging", "prod"]
          - name: "version"
            description: "Version to deploy"
            required: false
        tags: ["deployment", "ci-cd"]
    submenus:
      - name: "Git Operations"
        description: "Git-related scripts"
        scripts:
          - name: "git-cleanup"
            path: "~/scripts/git-cleanup.sh"
            description: "Clean up git branches"
            tags: ["git", "cleanup"]
```

## Integration with Doom Emacs

Add to your `config.el`:

```emacs-lisp
;; Load the desired script menu implementation
(load-file "~/org/literate-config/examples/menus/linustorv.el")

;; Add to your leader key map
(map! :leader
      (:prefix ("x" . "Scripts")
       :desc "Linus scripts" "l" #'linus-scripts
       :desc "Stallman scripts" "s" #'stallman-scripts
       :desc "Enhanced scripts" "e" #'magit-enhanced-scripts))
```

## Creating Your Own Implementation

1. **Choose your philosophy**: Minimalist, comprehensive, or visual
2. **Define your interface**: Transient, hydra, or custom
3. **Implement core functions**:
   - Script discovery
   - Execution with arguments
   - Menu navigation
   - Configuration management

4. **Add metadata support**:
   - Parse script comments
   - Extract arguments and help
   - Support tags and categories

## Performance Considerations

- **Caching**: Cache script discovery results
- **Lazy loading**: Load script metadata on demand  
- **Async execution**: Don't block Emacs during script execution
- **Memory usage**: Limit parsed content size

## Security Notes

- Always validate script paths
- Confirm execution of potentially dangerous scripts
- Consider sandboxing for untrusted scripts
- Use proper shell escaping for arguments

## Contributing

Each implementation demonstrates different approaches:
- **Minimalist**: Focus on core functionality
- **Comprehensive**: Full feature set with documentation
- **Visual**: Rich interface with exploration
- **Cross-platform**: Works outside Emacs

Choose the approach that fits your needs and extend as required.