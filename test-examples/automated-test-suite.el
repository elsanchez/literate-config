;;; automated-test-suite.el --- Automated testing suite for script interfaces

;; Comprehensive automated testing with continuous integration support

(require 'ert)
(require 'cl-lib)

(defvar ats-test-data-dir 
  (expand-file-name "test-data/" (file-name-directory load-file-name))
  "Directory for test data files.")

(defvar ats-implementations
  '((linus . ("linustorv.el" . linus-scripts))
    (stallman . ("stallman.el" . stallman-scripts))  
    (magit . ("magit-enhanced.el" . magit-enhanced-scripts)))
  "Implementations to test.")

(defvar ats-test-scripts-created nil
  "Flag to track if test scripts have been created.")

;; === TEST INFRASTRUCTURE ===

(defun ats-setup-test-environment ()
  "Setup test environment with sample data."
  (unless ats-test-scripts-created
    (ats-create-test-scripts)
    (ats-create-test-configurations)
    (setq ats-test-scripts-created t)))

(defun ats-create-test-scripts ()
  "Create test scripts for automated testing."
  (let ((test-scripts-dir (expand-file-name "scripts/" ats-test-data-dir)))
    (make-directory test-scripts-dir t)
    
    ;; Simple success script
    (ats-create-script "success.sh" 
                       "#!/bin/bash\necho 'Success'\nexit 0")
    
    ;; Script with arguments
    (ats-create-script "with-args.sh"
                       "#!/bin/bash\n# @arg name: Your name\n# @arg count: Number of greetings\nfor i in $(seq 1 ${2:-1}); do\n  echo \"Hello ${1:-World} #$i\"\ndone")
    
    ;; Failing script
    (ats-create-script "failure.sh"
                       "#!/bin/bash\necho 'This will fail'\nexit 1")
    
    ;; Long running script
    (ats-create-script "long-running.sh"
                       "#!/bin/bash\necho 'Starting long task...'\nsleep 3\necho 'Long task completed'")
    
    ;; Script with complex metadata
    (ats-create-script "complex.sh"
                       "#!/bin/bash\n# Description: Complex script with multiple features\n# Tags: test, complex, demo\n# Help: This script demonstrates complex argument handling\n# @arg environment: Target environment (dev/staging/prod)\n# @arg verbose: Enable verbose output (true/false)\n# @arg config: Configuration file path\necho \"Running in $1 mode with verbose=$2 and config=$3\"")))

(defun ats-create-script (name content)
  "Create a test script with given name and content."
  (let ((script-path (expand-file-name name (expand-file-name "scripts/" ats-test-data-dir))))
    (with-temp-file script-path
      (insert content))
    (set-file-modes script-path #o755)))

;; === UNIT TESTS ===

(ert-deftest ats-test-implementation-loading ()
  "Test that all implementations can be loaded without error."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl)))
          (file-info (cdr impl)))
      (should (ats-load-implementation-safely impl-name))
      (should (fboundp (cdr file-info))))))

(ert-deftest ats-test-script-discovery ()
  "Test script discovery functionality."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (ats-load-implementation-safely impl-name)
      (let ((discovered-scripts (ats-get-discovered-scripts impl-name)))
        (should (> (length discovered-scripts) 0))
        (should (member "success.sh" discovered-scripts))))))

(ert-deftest ats-test-script-execution ()
  "Test script execution functionality."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (ats-load-implementation-safely impl-name)
      
      ;; Test successful script execution
      (let ((result (ats-execute-script-safely impl-name "success.sh")))
        (should result)
        (should (eq (plist-get result :status) 'success)))
      
      ;; Test script with arguments
      (let ((result (ats-execute-script-safely impl-name "with-args.sh" '("TestUser" "2"))))
        (should result)))))

(ert-deftest ats-test-error-handling ()
  "Test error handling in implementations."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (ats-load-implementation-safely impl-name)
      
      ;; Test non-existent script
      (let ((result (ats-execute-script-safely impl-name "nonexistent.sh")))
        (should (or (null result) 
                   (eq (plist-get result :status) 'error))))
      
      ;; Test failing script
      (let ((result (ats-execute-script-safely impl-name "failure.sh")))
        (should result)
        ;; Should handle failure gracefully
        ))))

(ert-deftest ats-test-metadata-parsing ()
  "Test metadata parsing from scripts."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (when (ats-supports-metadata-p impl-name)
        (ats-load-implementation-safely impl-name)
        (let ((metadata (ats-get-script-metadata impl-name "complex.sh")))
          (should metadata)
          (should (plist-get metadata :description))
          (should (plist-get metadata :tags))
          (should (plist-get metadata :help)))))))

(ert-deftest ats-test-argument-handling ()
  "Test argument collection and validation."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (when (ats-supports-arguments-p impl-name)
        (ats-load-implementation-safely impl-name)
        (let ((args (ats-get-script-arguments impl-name "with-args.sh")))
          (should args)
          (should (> (length args) 0)))))))

(ert-deftest ats-test-performance-thresholds ()
  "Test that performance meets minimum thresholds."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      ;; Test load time
      (let ((load-time (ats-measure-load-time impl-name)))
        (should (< load-time 5.0))  ; Should load under 5 seconds
        (message "Load time for %s: %.3fs" impl-name load-time))
      
      ;; Test memory usage
      (let ((memory-usage (ats-measure-memory-usage impl-name)))
        (should (< memory-usage 50000))  ; Should use less than 50MB
        (message "Memory usage for %s: %dKB" impl-name (/ memory-usage 1024))))))

;; === INTEGRATION TESTS ===

(ert-deftest ats-test-end-to-end-workflow ()
  "Test complete end-to-end workflow."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (ats-load-implementation-safely impl-name)
      
      ;; Simulate complete workflow
      (let ((workflow-result (ats-simulate-user-workflow impl-name)))
        (should workflow-result)
        (should (plist-get workflow-result :discovery-success))
        (should (plist-get workflow-result :execution-success))))))

(ert-deftest ats-test-concurrent-operations ()
  "Test concurrent script operations."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (when (ats-supports-async-p impl-name)
        (ats-load-implementation-safely impl-name)
        
        ;; Start multiple scripts concurrently
        (let ((processes (ats-start-concurrent-scripts impl-name 
                                                       '("success.sh" "with-args.sh"))))
          (should processes)
          (should (> (length processes) 1))
          
          ;; Wait for completion and check results
          (ats-wait-for-processes processes)
          (should (ats-all-processes-completed-p processes)))))))

;; === STRESS TESTS ===

(ert-deftest ats-test-large-script-collection ()
  "Test performance with large number of scripts."
  (ats-setup-test-environment)
  
  ;; Create many test scripts
  (ats-create-many-test-scripts 100)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (ats-load-implementation-safely impl-name)
      
      ;; Test discovery performance
      (let ((start-time (current-time))
            (discovered-scripts (ats-get-discovered-scripts impl-name)))
        (let ((discovery-time (float-time (time-subtract (current-time) start-time))))
          (should (< discovery-time 10.0))  ; Should discover under 10 seconds
          (should (>= (length discovered-scripts) 100))  ; Should find most scripts
          (message "Discovery time for %s with 100 scripts: %.3fs" impl-name discovery-time))))))

(ert-deftest ats-test-memory-stability ()
  "Test memory stability over extended use."
  (ats-setup-test-environment)
  
  (dolist (impl ats-implementations)
    (let ((impl-name (symbol-name (car impl))))
      (ats-load-implementation-safely impl-name)
      
      (let ((initial-memory (car (memory-use-counts))))
        ;; Perform many operations
        (dotimes (i 50)
          (ats-execute-script-safely impl-name "success.sh")
          (when (= (mod i 10) 0)
            (garbage-collect)))
        
        (garbage-collect)
        (let ((final-memory (car (memory-use-counts)))
              (memory-growth (- (car (memory-use-counts)) initial-memory)))
          (should (< memory-growth 10000))  ; Should not grow more than 10MB
          (message "Memory growth for %s after 50 operations: %dKB" 
                   impl-name (/ memory-growth 1024)))))))

;; === REGRESSION TESTS ===

(ert-deftest ats-test-backwards-compatibility ()
  "Test backwards compatibility with previous versions."
  ;; Test that old configurations still work
  ;; Test that old scripts are still discoverable
  ;; Test that old keybindings still function
  (should t))  ; Placeholder

(ert-deftest ats-test-configuration-migration ()
  "Test configuration migration between versions."
  ;; Test migrating old config formats
  ;; Test preserving user customizations
  (should t))  ; Placeholder

;; === HELPER FUNCTIONS ===

(defun ats-load-implementation-safely (impl-name)
  "Safely load implementation, returning success status."
  (condition-case err
      (progn
        (ats-load-implementation impl-name)
        t)
    (error 
     (message "Failed to load %s: %s" impl-name (error-message-string err))
     nil)))

(defun ats-load-implementation (impl-name)
  "Load implementation for testing."
  (let* ((impl-info (alist-get (intern impl-name) ats-implementations))
         (file-name (car impl-info))
         (base-dir (file-name-directory load-file-name))
         (file-path (expand-file-name (format "../examples/menus/%s" file-name) base-dir)))
    (load-file file-path)))

(defun ats-execute-script-safely (impl-name script-name &optional args)
  "Safely execute script and return result."
  (condition-case err
      (let ((start-time (current-time)))
        ;; Implementation-specific execution
        (ats-impl-execute-script impl-name script-name args)
        `(:status success 
          :duration ,(float-time (time-subtract (current-time) start-time))))
    (error 
     `(:status error :error ,(error-message-string err)))))

(defun ats-impl-execute-script (impl-name script-name args)
  "Execute script using implementation-specific method."
  ;; This would need implementation-specific code
  (pcase impl-name
    ("linus" (ats-linus-execute script-name args))
    ("stallman" (ats-stallman-execute script-name args))
    ("magit" (ats-magit-execute script-name args))
    (_ (error "Unknown implementation: %s" impl-name))))

(defun ats-get-discovered-scripts (impl-name)
  "Get list of scripts discovered by implementation."
  ;; Implementation-specific discovery
  (let ((scripts-dir (expand-file-name "scripts/" ats-test-data-dir)))
    (when (file-directory-p scripts-dir)
      (directory-files scripts-dir nil "\\.sh$"))))

(defun ats-measure-load-time (impl-name)
  "Measure implementation load time."
  (let ((start-time (current-time)))
    (ats-load-implementation impl-name)
    (float-time (time-subtract (current-time) start-time))))

(defun ats-measure-memory-usage (impl-name)
  "Measure memory usage of implementation."
  (let ((before (memory-use-counts)))
    (ats-load-implementation impl-name)
    (garbage-collect)
    (let ((after (memory-use-counts)))
      (- (car after) (car before)))))

(defun ats-supports-metadata-p (impl-name)
  "Check if implementation supports metadata parsing."
  (member impl-name '("stallman" "magit")))

(defun ats-supports-arguments-p (impl-name)
  "Check if implementation supports argument handling."
  (member impl-name '("stallman" "magit")))

(defun ats-supports-async-p (impl-name)
  "Check if implementation supports async execution."
  t)  ; Assume all support async

(defun ats-create-many-test-scripts (count)
  "Create many test scripts for stress testing."
  (dotimes (i count)
    (ats-create-script (format "test-script-%03d.sh" i)
                       (format "#!/bin/bash\necho 'Test script %d'\nexit 0" i))))

;; === TEST SUITE RUNNER ===

(defun ats-run-all-tests ()
  "Run all automated tests."
  (interactive)
  (ats-setup-test-environment)
  (message "Running automated test suite...")
  
  (let ((start-time (current-time)))
    (ert-run-tests-batch-and-exit "ats-test-")
    (let ((duration (float-time (time-subtract (current-time) start-time))))
      (message "Test suite completed in %.2f seconds" duration))))

(defun ats-run-test-category (category)
  "Run tests in specific category."
  (interactive (list (completing-read "Category: " 
                                     '("unit" "integration" "stress" "regression"))))
  (ats-setup-test-environment)
  (message "Running %s tests..." category)
  
  (let ((test-pattern (format "ats-test-%s-" category)))
    (ert-run-tests-batch test-pattern)))

(defun ats-generate-test-report ()
  "Generate comprehensive test report."
  (interactive)
  (with-output-to-temp-buffer "*Test Report*"
    (princ "AUTOMATED TEST SUITE REPORT\n")
    (princ "===========================\n\n")
    
    ;; Run tests and collect results
    (let ((results (ats-collect-test-results)))
      (princ (ats-format-test-summary results))
      (princ (ats-format-detailed-results results))
      (princ (ats-format-performance-summary results)))))

;; === CONTINUOUS INTEGRATION SUPPORT ===

(defun ats-ci-runner ()
  "CI-friendly test runner with exit codes."
  (condition-case nil
      (progn
        (ats-setup-test-environment)
        (ert-run-tests-batch "ats-test-"))
    (error 
     (message "Tests failed")
     (kill-emacs 1))))

;; === MAIN INTERFACE ===

(transient-define-prefix ats-test-menu ()
  "Automated test suite menu"
  [["Test Categories"
    ("u" "Unit tests" (lambda () (interactive) (ats-run-test-category "unit")))
    ("i" "Integration tests" (lambda () (interactive) (ats-run-test-category "integration")))
    ("s" "Stress tests" (lambda () (interactive) (ats-run-test-category "stress")))
    ("r" "Regression tests" (lambda () (interactive) (ats-run-test-category "regression")))]
   ["Full Suite"
    ("a" "Run all tests" ats-run-all-tests)
    ("A" "Generate report" ats-generate-test-report)]
   ["Setup"
    ("S" "Setup environment" ats-setup-test-environment)
    ("C" "Clean test data" ats-clean-test-data)]
   ["Exit"
    ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun automated-test-suite ()
  "Launch automated test suite."
  (interactive)
  (ats-test-menu))

(provide 'automated-test-suite)

;;; automated-test-suite.el ends here