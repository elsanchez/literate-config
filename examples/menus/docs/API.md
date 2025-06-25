# API Reference and Extension Guide

## 🔌 API Overview

Each implementation provides a consistent API for extension and integration. This document covers the public APIs, extension points, and integration patterns.

## 🏗️ Common API Patterns

All implementations follow these common patterns:

```emacs-lisp
;; Core functions
(*-scripts)                    ; Main entry point
(*-run-script)                 ; Interactive script runner
(*-edit-script)                ; Edit script
(*-new-script name)            ; Create new script
(*-discover-scripts)           ; Find available scripts
(*-execute-script script args) ; Execute with arguments
```

## 🔧 Linus Scripts API

### Core Functions

```emacs-lisp
;; Main interface
(linus-scripts)                ; Entry point
(ts-menu)                      ; Main menu

;; Script operations
(ts-run-script)                ; Interactive runner
(ts-edit-script)               ; Edit script
(ts-new-script name)           ; Create script

;; Utility functions
(ts--get-scripts)              ; Get available scripts
(ts--run script &optional args) ; Execute script
(ts-change-dir)                ; Change script directory
(ts-toggle-async)              ; Toggle async execution
```

### Configuration Variables

```emacs-lisp
(defvar ts-script-dir "~/bin"
  "Directory containing scripts.")

(defvar ts-shell "/bin/bash"
  "Default shell for execution.")

(defvar ts-async t
  "Execute scripts asynchronously.")

(defvar ts--history nil
  "Command history.")
```

### Extension Points

```emacs-lisp
;; Add custom script discovery
(defun ts--get-scripts-custom ()
  "Custom script discovery logic."
  ;; Your implementation
  )

;; Hook for script execution
(add-hook 'ts-before-execute-hook 'my-pre-execute-function)
(add-hook 'ts-after-execute-hook 'my-post-execute-function)

;; Custom menu entries
(transient-append-suffix 'ts-menu '(0 -1)
  '("x" "Custom action" my-custom-function))
```

### Example Extensions

```emacs-lisp
;; Add script backup functionality
(defun ts-backup-script ()
  "Backup selected script."
  (interactive)
  (let ((script (completing-read "Backup: " (ts--get-scripts))))
    (copy-file script (concat script ".bak"))
    (message "Backed up %s" script)))

;; Add to menu
(transient-append-suffix 'ts-menu '(1 -1)
  '("B" "Backup script" ts-backup-script))
```

## 📚 Stallman Scripts API

### Core Functions

```emacs-lisp
;; Main interface
(stallman-scripts)             ; Entry point
(stallman-scripts-menu)        ; Main menu

;; Script operations
(stallman-scripts-execute-script) ; Interactive execution
(stallman-scripts-edit-script)    ; Edit script
(stallman-scripts-create-new-script name) ; Create script

;; Information functions
(stallman-scripts-show-configuration) ; Show settings
(stallman-scripts-help)               ; Help system
```

### Customization System

```emacs-lisp
;; Customize group
(defgroup stallman-scripts nil
  "Script execution interface respecting user freedom."
  :group 'applications)

;; Customizable variables
(defcustom stallman-scripts-directory "~/scripts"
  "Directory containing user scripts."
  :type 'directory
  :group 'stallman-scripts)

;; Access customization
(customize-group 'stallman-scripts)
```

### Extension Points

```emacs-lisp
;; Custom script discovery
(defun stallman-scripts--find-executable-files-custom ()
  "Custom script discovery with additional logic."
  ;; Your implementation
  )

;; Custom metadata extraction
(defun stallman-scripts--extract-custom-field (file-path field)
  "Extract custom metadata field."
  ;; Your implementation
  )

;; Hook system
(add-hook 'stallman-scripts-before-execute-hook 'my-function)
(add-hook 'stallman-scripts-after-execute-hook 'my-function)
```

## ⚡ Magit Enhanced API

### Core Functions

```emacs-lisp
;; Main interface
(magit-enhanced-scripts)       ; Entry point
(me-main-menu)                 ; Main menu

;; Script operations
(me-run-script)                ; Interactive runner
(me-edit-script)               ; Edit script
(me-new-from-template)         ; Create from template

;; Discovery functions
(me--find-scripts-recursive dir) ; Recursive discovery
(me--analyze-script path)        ; Analyze script metadata
```

### Data Structures

```emacs-lisp
;; Script structure
(cl-defstruct me-script
  name path description args help tags)

;; Menu structure  
(cl-defstruct me-menu
  name description scripts submenus)
```

### Configuration

```emacs-lisp
(defvar me-config-file "~/.config/script-menus.json"
  "Configuration file for script menus.")

(defvar me-templates-dir "~/.config/script-templates"
  "Directory for script templates.")

;; JSON configuration format
{
  "directories": ["~/scripts", "~/bin"],
  "exclude_patterns": ["*.bak", "test_*"],
  "templates": {
    "bash": "template.sh.template",
    "python": "template.py.template"
  }
}
```

### Extension Points

```emacs-lisp
;; Custom metadata parsing
(defun me--extract-custom-metadata (file-path)
  "Extract custom metadata from script."
  ;; Your implementation
  )

;; Custom menu building
(defun me--build-custom-menu (scripts)
  "Build custom menu structure."
  ;; Your implementation
  )

;; Template variables
(defun me--substitute-custom-vars (content script-name)
  "Substitute custom template variables."
  ;; Your implementation
  )
```

## 🧬 Hybrid Implementation API

### Core Functions

```emacs-lisp
;; Main interface
(hybrid-scripts)               ; Entry point
(hybrid-scripts-menu)          ; Main menu

;; Intelligent functions
(hybrid-find-and-run)          ; AI-powered script finder
(hybrid-run-script)            ; Standard runner
(hybrid-search-scripts)        ; Search functionality

;; Advanced features
(hybrid--suggest-scripts context) ; Context-based suggestions
(hybrid--calculate-relevance-score script context) ; Relevance scoring
```

### Data Structures

```emacs-lisp
(cl-defstruct hybrid-script
  name path description args tags help 
  executable-p last-modified size)
```

### Configuration

```emacs-lisp
(defgroup hybrid-scripts nil
  "Hybrid script management system."
  :group 'applications)

(defcustom hybrid-scripts-directory "~/scripts"
  "Primary directory for scripts."
  :type 'directory)

(defcustom hybrid-scripts-additional-directories 
  '("~/.local/bin" "~/bin")
  "Additional directories to search."
  :type '(repeat directory))

(defcustom hybrid-scripts-cache-enabled t
  "Enable caching for better performance."
  :type 'boolean)
```

### Advanced Extension Points

```emacs-lisp
;; Custom relevance scoring
(defun hybrid--custom-relevance-score (script context)
  "Custom relevance calculation."
  ;; Your scoring logic
  )

;; Custom caching strategy
(defun hybrid--custom-cache-strategy ()
  "Custom caching implementation."
  ;; Your caching logic
  )

;; AI integration
(defun hybrid--ai-script-suggestions (context)
  "AI-powered script suggestions."
  ;; Your AI integration
  )
```

## 🐍 Python TUI API

### Core Classes

```python
class ScriptRunner:
    def __init__(self, config_path: str)
    def load_config(self) -> None
    def discover_scripts(self) -> List[Script]
    def show_main_menu(self) -> None
    def execute_script(self, script: Script) -> None

class Script:
    name: str
    path: str
    description: str
    args: List[ScriptArg]
    tags: List[str]
    help_text: str
```

### Configuration API

```python
# YAML configuration
config = {
    'menus': [
        {
            'name': 'Development',
            'description': 'Dev scripts',
            'scripts': [
                {
                    'name': 'deploy',
                    'path': '~/scripts/deploy.sh',
                    'args': [
                        {
                            'name': 'environment',
                            'description': 'Target environment',
                            'choices': ['dev', 'staging', 'prod']
                        }
                    ]
                }
            ]
        }
    ]
}
```

### Extension Points

```python
# Custom script analysis
def analyze_custom_script(script_path: Path) -> Optional[Script]:
    """Custom script analysis logic."""
    # Your implementation
    pass

# Custom UI elements
def create_custom_menu(scripts: List[Script]) -> None:
    """Custom menu creation."""
    # Your implementation
    pass

# Integration hooks
class CustomScriptRunner(ScriptRunner):
    def pre_execute_hook(self, script: Script) -> None:
        """Called before script execution."""
        pass
    
    def post_execute_hook(self, script: Script, result: Any) -> None:
        """Called after script execution."""
        pass
```

## 🔌 Integration Patterns

### Project Integration

```emacs-lisp
;; Integrate with project.el
(defun scripts-for-current-project ()
  "Show scripts for current project."
  (interactive)
  (when-let ((project (project-current)))
    (let ((script-dir (expand-file-name "scripts" (project-root project))))
      (setq ts-script-dir script-dir)
      (linus-scripts))))

(global-set-key (kbd "C-c p s") 'scripts-for-current-project)
```

### File Manager Integration

```emacs-lisp
;; Integrate with dired
(defun dired-run-script-on-file ()
  "Run script on file at point."
  (interactive)
  (let ((file (dired-get-filename))
        (script (completing-read "Script: " (ts--get-scripts))))
    (ts--run script (list file))))

;; Add to dired-mode-map
(define-key dired-mode-map (kbd "R") 'dired-run-script-on-file)
```

### Version Control Integration

```emacs-lisp
;; Integrate with magit
(defun magit-run-deployment-script ()
  "Run deployment script with current commit."
  (interactive)
  (let ((commit (magit-commit-at-point))
        (script "deploy.sh"))
    (ts--run script (list "--commit" commit))))

;; Add to magit popup
(magit-define-popup-action 'magit-commit-popup 
  ?D "Deploy" 'magit-run-deployment-script)
```

### External Tool Integration

```emacs-lisp
;; Integrate with external tools
(defun scripts-with-docker ()
  "Run scripts in Docker container."
  (interactive)
  (let ((container (read-string "Container: "))
        (script (completing-read "Script: " (ts--get-scripts))))
    (async-shell-command 
     (format "docker exec %s bash -c '%s'" container script))))
```

## 🛠️ Creating Custom Implementations

### Basic Implementation Template

```emacs-lisp
;;; my-scripts.el --- My custom script interface

(require 'transient)

;; Configuration
(defvar my-scripts-directory "~/my-scripts")
(defvar my-scripts-cache nil)

;; Core functions
(defun my-scripts--discover ()
  "Discover scripts in directory."
  ;; Implementation
  )

(defun my-scripts--execute (script args)
  "Execute script with arguments."
  ;; Implementation
  )

;; Main interface
(transient-define-prefix my-scripts-menu ()
  "My custom script interface"
  [["Actions"
    ("r" "Run" my-scripts-run)
    ("e" "Edit" my-scripts-edit)]
   ["Exit"
    ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun my-scripts ()
  "Launch my script interface."
  (interactive)
  (my-scripts-menu))

(provide 'my-scripts)
```

### Advanced Features Template

```emacs-lisp
;; Add caching
(defun my-scripts--with-cache (key function)
  "Execute function with caching."
  (or (alist-get key my-scripts-cache)
      (let ((result (funcall function)))
        (push (cons key result) my-scripts-cache)
        result)))

;; Add metadata support
(defun my-scripts--parse-metadata (file)
  "Parse metadata from script file."
  ;; Implementation
  )

;; Add templating
(defun my-scripts--create-from-template (template name)
  "Create script from template."
  ;; Implementation
  )

;; Add search
(defun my-scripts--search (query)
  "Search scripts by query."
  ;; Implementation
  )
```

## 📊 Testing Integration

### Adding Tests for Custom Implementation

```emacs-lisp
;; Add to automated test suite
(ert-deftest my-scripts-test-discovery ()
  "Test script discovery."
  (should (> (length (my-scripts--discover)) 0)))

;; Add to benchmark framework
(add-to-list 'atr-implementations
             '(my-impl . ("my-scripts.el" . my-scripts)))

;; Add to UX evaluation
(defun test-my-scripts ()
  "Test my implementation."
  (interactive)
  (my-scripts--setup-test-environment)
  (my-scripts))
```

### Performance Monitoring

```emacs-lisp
;; Add performance monitoring
(defun my-scripts--with-timing (function)
  "Execute function with timing."
  (let ((start-time (current-time)))
    (prog1 (funcall function)
      (message "Execution time: %.3fs" 
               (float-time (time-subtract (current-time) start-time))))))
```

## 🔗 External Integration APIs

### REST API Wrapper

```emacs-lisp
;; HTTP API for external integration
(defun scripts-api-list ()
  "Return JSON list of available scripts."
  (json-encode (mapcar (lambda (script)
                         `((name . ,(script-name script))
                           (path . ,(script-path script))
                           (description . ,(script-description script))))
                       (my-scripts--discover))))

;; Serve via simple HTTP server
(defun scripts-start-api-server (port)
  "Start HTTP API server."
  (start-process "scripts-api" nil 
                 "python" "-m" "http.server" (number-to-string port)))
```

### Command Line Interface

```bash
#!/bin/bash
# CLI wrapper for Emacs script interface

emacs --batch \
      --load "examples/menus/my-scripts.el" \
      --eval "(my-scripts--cli-execute \"$1\" \"$2\")"
```

### IDE Integration

```json
// VS Code extension integration
{
  "commands": [
    {
      "command": "scripts.run",
      "title": "Run Script",
      "category": "Scripts"
    }
  ],
  "keybindings": [
    {
      "command": "scripts.run", 
      "key": "ctrl+shift+r"
    }
  ]
}
```

---

**Next Steps**: 
1. Choose an implementation to extend
2. Add your custom functionality
3. Integrate with your workflow
4. Contribute improvements back to the project

For more examples, see the test framework implementations in `../../test-examples/`.