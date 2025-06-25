;;; vanilla-profile.el --- Vanilla Emacs profile for script menu testing

;; Minimal Emacs configuration for testing script interfaces
;; To use: emacs --load ~/path/to/vanilla-profile.el

;; === BASIC SETUP ===

(setq user-emacs-directory (file-name-directory load-file-name))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install required packages
(unless (package-installed-p 'transient)
  (package-refresh-contents)
  (package-install 'transient))

;; === LOAD SCRIPT MENU FRAMEWORKS ===

(let ((test-dir (file-name-directory load-file-name)))
  (message "Loading script menu test frameworks...")
  
  ;; Load test frameworks
  (load-file (expand-file-name "test-runner.el" test-dir))
  (load-file (expand-file-name "hybrid-implementation.el" test-dir))
  (load-file (expand-file-name "visual-dashboard.el" test-dir))
  
  ;; Setup test environment
  (test-runner-setup-test-environment)
  
  (message "✓ Script menu frameworks loaded!"))

;; === KEYBINDINGS ===

(global-set-key (kbd "C-c t") 'test-runner-menu)
(global-set-key (kbd "C-c h") 'hybrid-scripts)
(global-set-key (kbd "C-c d") 'visual-dashboard)
(global-set-key (kbd "C-c l") 'test-linus-scripts)
(global-set-key (kbd "C-c s") 'test-stallman-scripts)
(global-set-key (kbd "C-c m") 'test-magit-enhanced-scripts)

;; === WELCOME MESSAGE ===

(defun vanilla-profile-welcome ()
  "Show welcome message for vanilla profile."
  (with-output-to-temp-buffer "*Script Menu Testing*"
    (princ "🧪 SCRIPT MENU TESTING - VANILLA EMACS\n")
    (princ "=====================================\n\n")
    (princ "Welcome to script menu testing in vanilla Emacs!\n\n")
    (princ "🎯 Available commands:\n")
    (princ "  C-c t - Main test menu\n")
    (princ "  C-c h - Hybrid implementation (best UX)\n")
    (princ "  C-c d - Visual dashboard\n")
    (princ "  C-c l - Linus Scripts\n")
    (princ "  C-c s - Stallman Scripts\n")
    (princ "  C-c m - Magit Enhanced\n\n")
    (princ "📁 Test scripts: test-examples/scripts/\n")
    (princ "🚀 Start with: C-c h (hybrid implementation)\n\n")
    (princ "Happy testing! 🎉\n")))

;; Show welcome after init
(add-hook 'after-init-hook 'vanilla-profile-welcome)

;; === BASIC UI IMPROVEMENTS ===

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq inhibit-startup-screen t)

;; Better defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(delete-selection-mode 1)
(show-paren-mode 1)

(message "🧪 Vanilla profile loaded! Press C-c h to start with hybrid implementation.")

(provide 'vanilla-profile)

;;; vanilla-profile.el ends here