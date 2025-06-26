;;; simple-loader.el --- Simple loader for script menu examples

;; Safe, step-by-step loader for the script menu examples

(defun simple-loader-check-requirements ()
  "Check if requirements are met."
  (let ((missing '()))
    
    ;; Check for transient
    (unless (or (featurep 'transient) 
                (locate-library "transient"))
      (push "transient" missing))
    
    ;; Check for cl-lib
    (unless (or (featurep 'cl-lib)
                (locate-library "cl-lib"))
      (push "cl-lib" missing))
    
    (if missing
        (progn
          (message "❌ Missing packages: %s" (string-join missing ", "))
          (message "Install with: M-x package-install RET <package-name> RET")
          nil)
      (progn
        (message "✓ All requirements available")
        t))))

(defun simple-loader-install-packages ()
  "Install required packages."
  (interactive)
  (message "Installing required packages...")
  
  ;; Ensure package.el is set up
  (require 'package)
  (unless package-archive-contents
    (package-refresh-contents))
  
  ;; Install transient if not available
  (unless (package-installed-p 'transient)
    (message "Installing transient...")
    (package-install 'transient))
  
  ;; Load packages
  (require 'cl-lib)
  (require 'transient)
  
  (message "✓ Packages installed and loaded"))

(defun simple-loader-load-basic ()
  "Load basic test runner only."
  (interactive)
  (let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
    
    ;; Check requirements first
    (unless (simple-loader-check-requirements)
      (when (y-or-n-p "Install missing packages? ")
        (simple-loader-install-packages)))
    
    ;; Load basic test runner
    (condition-case err
        (let ((test-runner-file (expand-file-name "test-runner.el" base-dir)))
          (if (file-exists-p test-runner-file)
              (progn
                (load-file test-runner-file)
                (message "✓ Basic test runner loaded")
                (when (fboundp 'test-runner-setup-test-environment)
                  (test-runner-setup-test-environment)
                  (message "✓ Test environment ready"))
                (message "🚀 Try: M-x test-runner-menu"))
            (message "❌ test-runner.el not found")))
      (error 
       (message "❌ Error loading basic runner: %s" (error-message-string err))))))

(defun simple-loader-load-hybrid ()
  "Load hybrid implementation with error handling."
  (interactive)
  (let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
    
    ;; Ensure requirements
    (unless (and (featurep 'transient) (featurep 'cl-lib))
      (simple-loader-install-packages))
    
    ;; Load hybrid implementation
    (condition-case err
        (let ((hybrid-file (expand-file-name "hybrid-implementation.el" base-dir)))
          (if (file-exists-p hybrid-file)
              (progn
                (load-file hybrid-file)
                (message "✓ Hybrid implementation loaded")
                (message "🚀 Try: M-x hybrid-scripts"))
            (message "❌ hybrid-implementation.el not found")))
      (error 
       (message "❌ Error loading hybrid: %s" (error-message-string err))))))

(defun simple-loader-load-individual (implementation)
  "Load individual implementation safely."
  (interactive (list (completing-read "Implementation: " 
                                     '("linus" "stallman" "magit"))))
  (let* ((base-dir (file-name-directory (or load-file-name buffer-file-name)))
         (examples-dir (expand-file-name "../examples/menus/" base-dir))
         (file-map '(("linus" . "linustorv.el")
                    ("stallman" . "stallman.el") 
                    ("magit" . "magit-enhanced.el")))
         (file-name (alist-get implementation file-map nil nil #'string=))
         (full-path (expand-file-name file-name examples-dir)))
    
    (if (file-exists-p full-path)
        (condition-case err
            (progn
              (when (string= implementation "stallman")
                (require 'cl-lib)) ; stallman needs cl-lib
              (load-file full-path)
              (message "✓ %s implementation loaded" implementation)
              (message "🚀 Try: M-x %s-scripts" implementation))
          (error 
           (message "❌ Error loading %s: %s" implementation (error-message-string err))))
      (message "❌ File not found: %s" full-path))))

;; Safe menu system without transient
(defun simple-loader-menu ()
  "Simple menu without transient dependency."
  (interactive)
  (let ((choice (completing-read "Choose action: " 
                                '("Load basic test runner"
                                  "Load hybrid implementation" 
                                  "Load individual implementation"
                                  "Install packages"
                                  "Check requirements"))))
    (pcase choice
      ("Load basic test runner" (simple-loader-load-basic))
      ("Load hybrid implementation" (simple-loader-load-hybrid))
      ("Load individual implementation" (call-interactively 'simple-loader-load-individual))
      ("Install packages" (simple-loader-install-packages))
      ("Check requirements" (simple-loader-check-requirements)))))

(message "🛠️ Simple loader ready! Try: M-x simple-loader-menu")

(provide 'simple-loader)

;;; simple-loader.el ends here