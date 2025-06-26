;;; doom-sandbox-config.el --- Doom sandbox for script menu testing

;; Minimal Doom configuration for testing script interfaces
;; To use: emacs --init-directory ~/.config/doom-sandbox

;; === DOOM CORE SETUP ===

;; Bootstrap Doom in sandbox mode
(setq doom-private-dir (file-name-directory load-file-name))
(setq doom-cache-dir (expand-file-name ".cache/" doom-private-dir))
(setq doom-data-dir (expand-file-name ".local/" doom-private-dir))

;; Load Doom framework
(unless (file-exists-p (expand-file-name "doom.el" doom-private-dir))
  ;; If doom.el doesn't exist, create minimal bootstrap
  (message "Creating minimal Doom bootstrap for sandbox..."))

;; === SCRIPT MENU TESTING SETUP ===

;; Ensure required packages are available
(when (fboundp 'straight-use-package)
  (straight-use-package 'transient))

(require 'transient)
(require 'cl-lib)

;; Load all test frameworks safely
(let ((test-dir (file-name-directory load-file-name)))
  
  ;; Load transient fixes first
  (let ((fix-file (expand-file-name "fix-transient.el" test-dir)))
    (when (file-exists-p fix-file)
      (load-file fix-file)
      (when (fboundp 'fix-doom-transient)
        (fix-doom-transient))))
  
  ;; Load simple loader
  (let ((loader-file (expand-file-name "simple-loader.el" test-dir)))
    (when (file-exists-p loader-file)
      (load-file loader-file)))
  
  ;; Load test frameworks if available
  (dolist (framework '("test-runner.el" "hybrid-implementation.el" 
                       "visual-dashboard.el" "advanced-test-runner.el"))
    (let ((framework-file (expand-file-name framework test-dir)))
      (when (file-exists-p framework-file)
        (condition-case err
            (load-file framework-file)
          (error
           (message "Warning: Failed to load %s: %s" framework (error-message-string err))))))))

;; Setup test environment if function exists
(when (fboundp 'test-runner-setup-test-environment)
  (test-runner-setup-test-environment))

;; === SANDBOX-SPECIFIC CONFIGURATION ===

;; Quick access keybindings
(map! :leader
      (:prefix ("t" . "🧪 Script Testing")
       :desc "Simple Loader Menu" "t" #'simple-loader-menu
       :desc "Load Hybrid" "h" #'simple-loader-load-hybrid
       :desc "Load Basic" "b" #'simple-loader-load-basic
       :desc "Fix Transient" "f" #'fix-doom-transient
       :desc "Load Individual" "i" #'simple-loader-load-individual
       (:when (fboundp 'test-runner-menu)
        :desc "Test Runner Menu" "r" #'test-runner-menu)
       (:when (fboundp 'hybrid-scripts)
        :desc "Hybrid Scripts" "H" #'hybrid-scripts)
       (:when (fboundp 'visual-dashboard)
        :desc "Visual Dashboard" "d" #'visual-dashboard)
       (:when (fboundp 'advanced-test-runner)
        :desc "Advanced Runner" "a" #'advanced-test-runner)
       (:when (fboundp 'test-linus-scripts)
        :desc "Linus Scripts" "l" #'test-linus-scripts)
       (:when (fboundp 'test-stallman-scripts)
        :desc "Stallman Scripts" "s" #'test-stallman-scripts)
       (:when (fboundp 'test-magit-enhanced-scripts)
        :desc "Magit Enhanced" "m" #'test-magit-enhanced-scripts)))

;; Sandbox welcome message
(defun sandbox-welcome ()
  "Show sandbox welcome message."
  (interactive)
  (with-output-to-temp-buffer "*Sandbox Welcome*"
    (princ "🏖️  SCRIPT MENU SANDBOX\n")
    (princ "============================\n\n")
    (princ "Welcome to the Script Menu Testing Sandbox!\n\n")
    (princ "🎯 Quick Start:\n")
    (princ "  SPC t t - Simple loader menu\n")
    (princ "  SPC t h - Load hybrid implementation (best UX)\n")
    (princ "  SPC t b - Load basic framework\n")
    (princ "  SPC t f - Fix transient issues\n")
    (princ "  SPC t i - Load individual implementation\n\n")
    (princ "🔬 Advanced (if available):\n")
    (princ "  SPC t r - Test runner menu\n")
    (princ "  SPC t d - Visual dashboard\n")
    (princ "  SPC t a - Advanced benchmarking\n")
    (princ "  SPC t l - Linus Scripts (performance)\n")
    (princ "  SPC t s - Stallman Scripts (comprehensive)\n")
    (princ "  SPC t m - Magit Enhanced (visual)\n\n")
    (princ "📁 Test scripts location: ~/org/literate-config/test-examples/scripts/\n")
    (princ "📊 Results will be saved to ~/.config/script-interface-evaluation.json\n\n")
    (princ "Have fun exploring! 🚀\n")))

;; Auto-show welcome on startup
(add-hook 'doom-init-ui-hook #'sandbox-welcome)

;; === SANDBOX UI TWEAKS ===

;; Customize for testing
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Fira Code" :size 14))

;; Dashboard customization
(setq +doom-dashboard-banner-file nil)
(setq +doom-dashboard-banner-dir nil)
(setq doom-dashboard-banner-padding '(0 . 0))

;; Custom dashboard
(defun +doom-dashboard-widget-sandbox ()
  (insert
   "\n"
   (propertize "🏖️ SCRIPT MENU SANDBOX" 'face 'doom-dashboard-banner)
   "\n\n"
   (propertize "Press SPC t t to start testing!" 'face 'doom-dashboard-menu-title)
   "\n"))

(add-to-list '+doom-dashboard-functions '+doom-dashboard-widget-sandbox)

(message "🏖️ Doom Sandbox loaded! Press SPC t t to start testing.")

(provide 'doom-sandbox-config)

;;; doom-sandbox-config.el ends here