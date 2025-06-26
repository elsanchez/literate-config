;;; fix-transient.el --- Fix transient loading issues

;; This file ensures transient is properly loaded before using the examples

(defun ensure-transient-loaded ()
  "Ensure transient package is available and loaded."
  (interactive)
  (condition-case err
      (progn
        ;; Try to require transient
        (require 'transient nil t)
        
        ;; If not available, try to install it
        (unless (featurep 'transient)
          (message "Installing transient package...")
          (unless (package-installed-p 'transient)
            (package-refresh-contents)
            (package-install 'transient))
          (require 'transient))
        
        ;; Verify it's working
        (if (fboundp 'transient-define-prefix)
            (message "✓ Transient loaded successfully")
          (error "Transient loaded but transient-define-prefix not available")))
    
    (error 
     (message "❌ Error loading transient: %s" (error-message-string err))
     (message "Try installing manually: M-x package-install RET transient RET"))))

(defun fix-doom-transient ()
  "Fix transient in Doom Emacs environment."
  (interactive)
  (condition-case err
      (progn
        ;; In Doom, try to load transient module
        (when (bound-and-true-p doom-version)
          (message "Detected Doom Emacs, attempting to load transient...")
          
          ;; Try different ways to load transient in Doom
          (or (ignore-errors (require 'transient))
              (ignore-errors (load "transient"))
              (ignore-errors 
                (when (fboundp 'use-package)
                  (use-package transient :ensure t)))
              (progn
                (message "Installing transient via package.el...")
                (package-install 'transient)
                (require 'transient))))
        
        (if (fboundp 'transient-define-prefix)
            (message "✓ Transient fixed for Doom")
          (error "Still having issues with transient")))
    
    (error 
     (message "❌ Error fixing Doom transient: %s" (error-message-string err)))))

(defun smart-load-examples ()
  "Smart loading of script menu examples with error handling."
  (interactive)
  (let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
    
    ;; Ensure transient is loaded
    (ensure-transient-loaded)
    
    ;; Wait a moment for transient to be ready
    (sit-for 0.1)
    
    ;; Load examples with error handling
    (dolist (file '("test-runner.el" "hybrid-implementation.el"))
      (let ((full-path (expand-file-name file base-dir)))
        (condition-case err
            (progn
              (message "Loading %s..." file)
              (load-file full-path)
              (message "✓ Loaded %s" file))
          (error 
           (message "❌ Error loading %s: %s" file (error-message-string err))))))
    
    ;; Setup test environment
    (condition-case err
        (when (fboundp 'test-runner-setup-test-environment)
          (test-runner-setup-test-environment)
          (message "✓ Test environment ready"))
      (error 
       (message "❌ Error setting up test environment: %s" (error-message-string err))))
    
    (message "🚀 Ready! Try: (test-runner-menu) or (hybrid-scripts)")))

;; Auto-fix when loading
(ensure-transient-loaded)

(provide 'fix-transient)

;;; fix-transient.el ends here