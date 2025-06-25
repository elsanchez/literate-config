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

;; Load all test frameworks
(let ((test-dir (file-name-directory load-file-name)))
  
  ;; Core test runner
  (load-file (expand-file-name "test-runner.el" test-dir))
  
  ;; Advanced frameworks
  (load-file (expand-file-name "hybrid-implementation.el" test-dir))
  (load-file (expand-file-name "visual-dashboard.el" test-dir))
  (load-file (expand-file-name "advanced-test-runner.el" test-dir))
  
  ;; Setup test environment
  (test-runner-setup-test-environment))

;; === SANDBOX-SPECIFIC CONFIGURATION ===

;; Quick access keybindings
(map! :leader
      (:prefix ("t" . "🧪 Script Testing")
       :desc "Test Menu" "t" #'test-runner-menu
       :desc "Hybrid Scripts" "h" #'hybrid-scripts
       :desc "Visual Dashboard" "d" #'visual-dashboard
       :desc "Advanced Runner" "a" #'advanced-test-runner
       :desc "Linus Scripts" "l" #'test-linus-scripts
       :desc "Stallman Scripts" "s" #'test-stallman-scripts
       :desc "Magit Enhanced" "m" #'test-magit-enhanced-scripts))

;; Sandbox welcome message
(defun sandbox-welcome ()
  "Show sandbox welcome message."
  (interactive)
  (with-output-to-temp-buffer "*Sandbox Welcome*"
    (princ "🏖️  SCRIPT MENU SANDBOX\n")
    (princ "============================\n\n")
    (princ "Welcome to the Script Menu Testing Sandbox!\n\n")
    (princ "🎯 Quick Start:\n")
    (princ "  SPC t t - Main test menu\n")
    (princ "  SPC t h - Hybrid implementation (best UX)\n")
    (princ "  SPC t d - Visual dashboard\n")
    (princ "  SPC t a - Advanced benchmarking\n\n")
    (princ "🔬 Individual implementations:\n")
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