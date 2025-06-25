;;; test-runner.el --- Test runner for script menu examples

;; Test runner for exploring different script menu implementations
;; Load this file and call functions directly to test each example

(defvar test-runner-base-dir 
  (file-name-directory (or load-file-name buffer-file-name))
  "Base directory for test runner.")

(defvar test-runner-examples-dir 
  (expand-file-name "../examples/menus/" test-runner-base-dir)
  "Directory containing example implementations.")

(defvar test-runner-scripts-dir
  (expand-file-name "scripts/" test-runner-base-dir)
  "Directory for test scripts.")

;; Load all example implementations
(defun test-runner-load-examples ()
  "Load all example script menu implementations."
  (interactive)
  (let ((examples '("linustorv.el" "stallman.el" "magit-enhanced.el")))
    (dolist (example examples)
      (let ((file (expand-file-name example test-runner-examples-dir)))
        (if (file-exists-p file)
            (progn
              (load-file file)
              (message "✓ Loaded %s" example))
          (message "✗ Not found: %s" file))))))

;; Test functions for each implementation
;;;###autoload
(defun test-linus-scripts ()
  "Test Linus Torvalds style script menu."
  (interactive)
  (test-runner-ensure-examples-loaded)
  (test-runner-setup-test-environment)
  (if (fboundp 'linus-scripts)
      (progn
        (message "🔧 Starting Linus Scripts test...")
        (linus-scripts))
    (message "❌ linus-scripts function not available. Load examples first.")))

;;;###autoload
(defun test-stallman-scripts ()
  "Test Richard Stallman style script menu."
  (interactive)
  (test-runner-ensure-examples-loaded)
  (test-runner-setup-test-environment)
  (if (fboundp 'stallman-scripts)
      (progn
        (message "📚 Starting Stallman Scripts test...")
        (stallman-scripts))
    (message "❌ stallman-scripts function not available. Load examples first.")))

;;;###autoload
(defun test-magit-enhanced-scripts ()
  "Test Magit-enhanced style script menu."
  (interactive)
  (test-runner-ensure-examples-loaded)
  (test-runner-setup-test-environment)
  (if (fboundp 'magit-enhanced-scripts)
      (progn
        (message "⚡ Starting Magit Enhanced Scripts test...")
        (magit-enhanced-scripts))
    (message "❌ magit-enhanced-scripts function not available. Load examples first.")))

;;;###autoload
(defun test-python-runner ()
  "Test Python TUI script runner."
  (interactive)
  (test-runner-setup-test-environment)
  (let* ((python-script (expand-file-name "../examples/menus/script_runner.py" test-runner-base-dir))
         (config-file (expand-file-name "configs/test-runner.yaml" test-runner-base-dir)))
    (if (file-exists-p python-script)
        (progn
          (message "🐍 Starting Python Script Runner test...")
          (async-shell-command (format "%s --config %s" python-script config-file)))
      (message "❌ Python script runner not found."))))

;; Helper functions
(defun test-runner-ensure-examples-loaded ()
  "Ensure all examples are loaded."
  (unless (and (fboundp 'linus-scripts)
               (fboundp 'stallman-scripts)
               (fboundp 'magit-enhanced-scripts))
    (test-runner-load-examples)))

(defun test-runner-setup-test-environment ()
  "Setup test environment with sample scripts."
  (interactive)
  (test-runner-create-sample-scripts)
  (test-runner-create-test-configs)
  (message "✓ Test environment ready"))

(defun test-runner-create-sample-scripts ()
  "Create sample scripts for testing."
  (unless (file-exists-p test-runner-scripts-dir)
    (make-directory test-runner-scripts-dir t))
  
  ;; Create sample scripts
  (let ((scripts '(
    ("deploy.sh" . "#!/bin/bash\n# Description: Deploy application to target environment\n# Tags: deployment, ci-cd\n# @arg environment: Target environment (dev/staging/prod)\n# @arg version: Version to deploy (optional)\n\necho \"Deploying to $1 with version ${2:-latest}\"\nsleep 2\necho \"✓ Deployment completed\"")
    
    ("test.sh" . "#!/bin/bash\n# Description: Run application tests\n# Tags: testing, ci-cd\n# @arg type: Test type (unit/integration/e2e)\n# @arg coverage: Generate coverage report (true/false)\n\necho \"Running $1 tests...\"\nif [ \"$2\" = \"true\" ]; then\n  echo \"Generating coverage report...\"\nfi\nsleep 3\necho \"✓ Tests completed\"")
    
    ("build.sh" . "#!/bin/bash\n# Description: Build application\n# Tags: build, ci-cd\n# @arg target: Build target (dev/prod)\n# @arg clean: Clean before build (true/false)\n\nif [ \"$2\" = \"true\" ]; then\n  echo \"Cleaning previous build...\"\nfi\necho \"Building for $1...\"\nsleep 2\necho \"✓ Build completed\"")
    
    ("git-status.sh" . "#!/bin/bash\n# Description: Enhanced git status\n# Tags: git, status\n\necho \"=== Git Status ===\"\ngit status --porcelain\necho \"\"\necho \"=== Branch Info ===\"\ngit branch -v")
    
    ("git-cleanup.sh" . "#!/bin/bash\n# Description: Clean up merged git branches\n# Tags: git, cleanup\n# @arg dry-run: Show what would be deleted (true/false)\n\nif [ \"$1\" = \"true\" ]; then\n  echo \"DRY RUN: Would delete:\"\n  git branch --merged | grep -v master | grep -v main\nelse\n  echo \"Deleting merged branches...\"\n  git branch --merged | grep -v master | grep -v main | xargs -n 1 git branch -d\nfi")
    
    ("backup.sh" . "#!/bin/bash\n# Description: Backup important files\n# Tags: backup, maintenance\n# @arg destination: Backup destination path\n# @arg compress: Compress backup (true/false)\n\necho \"Creating backup to $1...\"\nif [ \"$2\" = \"true\" ]; then\n  echo \"Using compression...\"\nfi\nsleep 1\necho \"✓ Backup completed\"")
    )))
    
    (dolist (script scripts)
      (let ((file (expand-file-name (car script) test-runner-scripts-dir)))
        (with-temp-file file
          (insert (cdr script)))
        (set-file-modes file #o755)))))

(defun test-runner-create-test-configs ()
  "Create test configuration files."
  (let ((config-dir (expand-file-name "configs/" test-runner-base-dir)))
    (unless (file-exists-p config-dir)
      (make-directory config-dir t))
    
    ;; Python runner config
    (let ((python-config (expand-file-name "test-runner.yaml" config-dir)))
      (with-temp-file python-config
        (insert "menus:\n")
        (insert "  - name: \"Development\"\n")
        (insert "    description: \"Development and CI/CD scripts\"\n")
        (insert "    scripts:\n")
        (insert "      - name: \"deploy\"\n")
        (insert (format "        path: \"%s/deploy.sh\"\n" test-runner-scripts-dir))
        (insert "        description: \"Deploy application\"\n")
        (insert "        args:\n")
        (insert "          - name: \"environment\"\n")
        (insert "            description: \"Target environment\"\n")
        (insert "            choices: [\"dev\", \"staging\", \"prod\"]\n")
        (insert "          - name: \"version\"\n")
        (insert "            description: \"Version to deploy\"\n")
        (insert "            required: false\n")
        (insert "        tags: [\"deployment\", \"ci-cd\"]\n")
        (insert "      - name: \"test\"\n")
        (insert (format "        path: \"%s/test.sh\"\n" test-runner-scripts-dir))
        (insert "        description: \"Run tests\"\n")
        (insert "        tags: [\"testing\"]\n")
        (insert "    submenus:\n")
        (insert "      - name: \"Git Operations\"\n")
        (insert "        description: \"Git-related scripts\"\n")
        (insert "        scripts:\n")
        (insert "          - name: \"git-status\"\n")
        (insert (format "            path: \"%s/git-status.sh\"\n" test-runner-scripts-dir))
        (insert "            description: \"Enhanced git status\"\n")
        (insert "            tags: [\"git\"]\n")
        (insert "          - name: \"git-cleanup\"\n")
        (insert (format "            path: \"%s/git-cleanup.sh\"\n" test-runner-scripts-dir))
        (insert "            description: \"Clean up branches\"\n")
        (insert "            tags: [\"git\", \"cleanup\"]\n")))
    
    ;; Update script directories for Emacs implementations
    (when (boundp 'ts-script-dir)
      (setq ts-script-dir test-runner-scripts-dir))
    (when (boundp 'stallman-scripts-directory)
      (setq stallman-scripts-directory test-runner-scripts-dir))))

;; Main test menu
(transient-define-prefix test-runner-menu ()
  "Test menu for all script runner implementations"
  [["Test Implementations"
    ("l" "Linus Scripts" test-linus-scripts)
    ("s" "Stallman Scripts" test-stallman-scripts)
    ("m" "Magit Enhanced" test-magit-enhanced-scripts)
    ("p" "Python Runner" test-python-runner)]
   ["Setup & Management"
    ("L" "Load examples" test-runner-load-examples)
    ("S" "Setup environment" test-runner-setup-test-environment)
    ("d" "Open scripts directory" (lambda () (interactive) (dired test-runner-scripts-dir)))
    ("c" "Open configs directory" (lambda () (interactive) (dired (expand-file-name "configs/" test-runner-base-dir))))]
   ["Help & Info"
    ("h" "Show help" test-runner-show-help)
    ("?" "Show functions" test-runner-show-functions)]
   ["Exit"
    ("q" "quit" transient-quit-one)]])

(defun test-runner-show-help ()
  "Show test runner help."
  (interactive)
  (with-help-window "*Test Runner Help*"
    (princ "Script Menu Test Runner\n")
    (princ "======================\n\n")
    (princ "This test runner allows you to explore different script menu implementations:\n\n")
    (princ "Implementations:\n")
    (princ "  l - Linus Scripts: Pragmatic, direct approach\n")
    (princ "  s - Stallman Scripts: Freedom-focused, comprehensive\n")
    (princ "  m - Magit Enhanced: Visual, feature-rich interface\n")
    (princ "  p - Python Runner: Cross-platform TUI with YAML config\n\n")
    (princ "Setup:\n")
    (princ "  L - Load all example implementations\n")
    (princ "  S - Setup test environment with sample scripts\n\n")
    (princ "Sample scripts include:\n")
    (princ "  - deploy.sh: Application deployment\n")
    (princ "  - test.sh: Test runner\n")
    (princ "  - build.sh: Build system\n")
    (princ "  - git-*.sh: Git operations\n")
    (princ "  - backup.sh: File backup\n\n")
    (princ (format "Scripts directory: %s\n" test-runner-scripts-dir))
    (princ (format "Configs directory: %s\n" (expand-file-name "configs/" test-runner-base-dir)))))

(defun test-runner-show-functions ()
  "Show available test functions."
  (interactive)
  (with-help-window "*Test Functions*"
    (princ "Available Test Functions:\n")
    (princ "========================\n\n")
    (princ "Direct function calls:\n")
    (princ "  (test-linus-scripts)\n")
    (princ "  (test-stallman-scripts)\n")
    (princ "  (test-magit-enhanced-scripts)\n")
    (princ "  (test-python-runner)\n\n")
    (princ "Setup functions:\n")
    (princ "  (test-runner-load-examples)\n")
    (princ "  (test-runner-setup-test-environment)\n\n")
    (princ "Main menu:\n")
    (princ "  (test-runner-menu)\n\n")
    (princ "Usage:\n")
    (princ "1. Load this file: (load-file \"test-examples/test-runner.el\")\n")
    (princ "2. Setup environment: (test-runner-setup-test-environment)\n")
    (princ "3. Test any implementation: (test-linus-scripts)\n")
    (princ "4. Or use the menu: (test-runner-menu)\n")))

;; Auto-setup when loading
(defun test-runner-initialize ()
  "Initialize test runner."
  (message "🧪 Test Runner loaded")
  (message "📁 Scripts directory: %s" test-runner-scripts-dir)
  (message "⚙️  Run (test-runner-menu) to start testing")
  (message "🔧 Run (test-runner-setup-test-environment) to create sample scripts"))

;; Initialize when loaded
(test-runner-initialize)

;;;###autoload
(defun test-runner ()
  "Main entry point for test runner."
  (interactive)
  (test-runner-menu))

(provide 'test-runner)

;;; test-runner.el ends here