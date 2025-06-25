;;; hybrid-implementation.el --- Optimal script interface combining best features

;; AI-generated hybrid implementation taking the best features from each approach:
;; - Linus: Performance and directness
;; - Stallman: Comprehensive help and customization
;; - Magit: Visual design and discovery
;; - Python: Cross-platform compatibility and rich interface

(require 'transient)
(require 'json)
(require 'cl-lib)

;; === CORE CONFIGURATION ===

(defgroup hybrid-scripts nil
  "Hybrid script management system combining best practices."
  :group 'applications
  :prefix "hybrid-scripts-")

(defcustom hybrid-scripts-directory "~/scripts"
  "Primary directory for scripts."
  :type 'directory
  :group 'hybrid-scripts)

(defcustom hybrid-scripts-additional-directories '("~/.local/bin" "~/bin")
  "Additional directories to search for scripts."
  :type '(repeat directory)
  :group 'hybrid-scripts)

(defcustom hybrid-scripts-shell "/bin/bash"
  "Default shell for script execution."
  :type 'string
  :group 'hybrid-scripts)

(defcustom hybrid-scripts-async-by-default t
  "Execute scripts asynchronously by default."
  :type 'boolean
  :group 'hybrid-scripts)

(defcustom hybrid-scripts-auto-discovery t
  "Automatically discover scripts in configured directories."
  :type 'boolean
  :group 'hybrid-scripts)

(defcustom hybrid-scripts-cache-enabled t
  "Enable caching for better performance."
  :type 'boolean
  :group 'hybrid-scripts)

(defcustom hybrid-scripts-cache-duration 300
  "Cache duration in seconds."
  :type 'integer
  :group 'hybrid-scripts)

;; === PERFORMANCE-OPTIMIZED CORE (from Linus approach) ===

(defvar hybrid--script-cache nil
  "Cache for discovered scripts.")

(defvar hybrid--cache-timestamp nil
  "Timestamp of last cache update.")

(defvar hybrid--discovery-in-progress nil
  "Flag to prevent concurrent discovery operations.")

(cl-defstruct hybrid-script
  name path description args tags help executable-p last-modified size)

(defun hybrid--cache-valid-p ()
  "Check if cache is still valid."
  (and hybrid--cache-timestamp
       hybrid-scripts-cache-enabled
       (< (float-time (time-subtract (current-time) hybrid--cache-timestamp))
          hybrid-scripts-cache-duration)))

(defun hybrid--get-scripts-cached ()
  "Get scripts with caching for performance."
  (if (hybrid--cache-valid-p)
      hybrid--script-cache
    (hybrid--discover-scripts-with-caching)))

(defun hybrid--discover-scripts-with-caching ()
  "Discover scripts and update cache."
  (unless hybrid--discovery-in-progress
    (setq hybrid--discovery-in-progress t)
    (unwind-protect
        (progn
          (setq hybrid--script-cache (hybrid--discover-all-scripts))
          (setq hybrid--cache-timestamp (current-time)))
      (setq hybrid--discovery-in-progress nil)))
  hybrid--script-cache)

;; === COMPREHENSIVE DISCOVERY (from Magit approach) ===

(defun hybrid--discover-all-scripts ()
  "Discover all scripts from configured directories."
  (let ((all-scripts '())
        (directories (cons hybrid-scripts-directory 
                          hybrid-scripts-additional-directories)))
    
    (dolist (dir directories)
      (when (file-directory-p (expand-file-name dir))
        (let ((scripts (hybrid--discover-scripts-in-directory dir)))
          (setq all-scripts (append all-scripts scripts)))))
    
    ;; Remove duplicates and sort
    (hybrid--deduplicate-and-sort all-scripts)))

(defun hybrid--discover-scripts-in-directory (directory)
  "Discover scripts in specific directory with metadata parsing."
  (let ((scripts '())
        (dir-path (expand-file-name directory)))
    
    (dolist (file (directory-files-recursively dir-path ".*" nil t))
      (when (and (file-regular-p file)
                 (file-executable-p file)
                 (not (string-match-p "/\\." file))  ; Skip hidden files
                 (hybrid--looks-like-script-p file))
        (let ((script (hybrid--create-script-from-file file)))
          (when script
            (push script scripts)))))
    
    scripts))

(defun hybrid--looks-like-script-p (file)
  "Check if file looks like a script."
  (or (string-match-p "\\.[sh|py|pl|rb|js]$" file)
      (hybrid--has-shebang-p file)))

(defun hybrid--has-shebang-p (file)
  "Check if file has shebang line."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 100)
        (goto-char (point-min))
        (looking-at "#!"))
    (error nil)))

(defun hybrid--create-script-from-file (file-path)
  "Create script object from file with metadata parsing."
  (condition-case err
      (let* ((relative-path (file-relative-name file-path hybrid-scripts-directory))
             (name (file-name-nondirectory file-path))
             (stats (file-attributes file-path))
             (last-modified (nth 5 stats))
             (size (nth 7 stats)))
        
        (make-hybrid-script
         :name name
         :path file-path
         :description (hybrid--extract-description file-path)
         :args (hybrid--extract-arguments file-path)
         :tags (hybrid--extract-tags file-path)
         :help (hybrid--extract-help file-path)
         :executable-p (file-executable-p file-path)
         :last-modified last-modified
         :size size))
    (error 
     (message "Error processing script %s: %s" file-path (error-message-string err))
     nil)))

;; === METADATA PARSING (from Stallman approach) ===

(defun hybrid--extract-description (file-path)
  "Extract description from script comments."
  (hybrid--extract-field file-path "Description"))

(defun hybrid--extract-arguments (file-path)
  "Extract argument definitions from script."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path nil 0 2000)
        (let ((args '()))
          (goto-char (point-min))
          (while (re-search-forward "^#\\s-*@arg\\s-+\\([^:]+\\):\\s-*\\(.*\\)$" nil t)
            (let ((arg-name (match-string 1))
                  (arg-desc (match-string 2)))
              (push `(:name ,arg-name :description ,arg-desc) args)))
          (nreverse args)))
    (error nil)))

(defun hybrid--extract-tags (file-path)
  "Extract tags from script comments."
  (let ((tags-string (hybrid--extract-field file-path "Tags")))
    (when tags-string
      (split-string tags-string "[, ]+" t))))

(defun hybrid--extract-help (file-path)
  "Extract help text from script comments."
  (hybrid--extract-field file-path "Help"))

(defun hybrid--extract-field (file-path field)
  "Extract specific field from script comments."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path nil 0 1000)
        (goto-char (point-min))
        (when (re-search-forward (format "^#\\s-*%s:\\s-*\\(.*\\)$" field) nil t)
          (string-trim (match-string 1))))
    (error nil)))

;; === VISUAL INTERFACE (from Magit approach) ===

(defun hybrid--group-scripts-by-tags (scripts)
  "Group scripts by their tags for better organization."
  (let ((groups (make-hash-table :test 'equal))
        (ungrouped '()))
    
    (dolist (script scripts)
      (let ((tags (hybrid-script-tags script)))
        (if tags
            (dolist (tag tags)
              (puthash tag (cons script (gethash tag groups)) groups))
          (push script ungrouped))))
    
    ;; Convert hash table to alist and add ungrouped
    (let ((result (hash-table-alist groups)))
      (when ungrouped
        (push (cons "Other" ungrouped) result))
      result)))

(defun hybrid--format-script-info (script)
  "Format script information for display."
  (let ((name (hybrid-script-name script))
        (desc (or (hybrid-script-description script) "No description"))
        (tags (hybrid-script-tags script)))
    (format "%s%s%s" 
            name
            (if desc (format " - %s" desc) "")
            (if tags (format " [%s]" (string-join tags ", ")) ""))))

;; === EXECUTION ENGINE (optimized from all approaches) ===

(defun hybrid--execute-script (script &optional args-override)
  "Execute script with optimized argument collection and error handling."
  (let* ((script-path (hybrid-script-path script))
         (script-args (or args-override (hybrid--collect-arguments script)))
         (execution-method (if hybrid-scripts-async-by-default 'async 'sync)))
    
    (hybrid--validate-script-execution script script-args)
    (hybrid--record-execution-start script)
    
    (condition-case err
        (hybrid--do-execute-script script script-args execution-method)
      (error 
       (hybrid--handle-execution-error script err)))))

(defun hybrid--collect-arguments (script)
  "Collect arguments for script execution with smart defaults."
  (let ((args (hybrid-script-args script))
        (collected-args '()))
    
    (dolist (arg args)
      (let* ((arg-name (plist-get arg :name))
             (arg-desc (plist-get arg :description))
             (prompt (format "%s (%s): " arg-name arg-desc))
             (value (read-string prompt)))
        (when (not (string-empty-p value))
          (push (format "--%s=%s" arg-name value) collected-args))))
    
    (nreverse collected-args)))

(defun hybrid--do-execute-script (script args method)
  "Execute script using specified method."
  (let* ((script-path (hybrid-script-path script))
         (command (format "%s %s %s" 
                         hybrid-scripts-shell
                         (shell-quote-argument script-path)
                         (string-join args " "))))
    
    (message "Executing: %s" (hybrid-script-name script))
    
    (pcase method
      ('async (hybrid--execute-async command script))
      ('sync (hybrid--execute-sync command script))
      (_ (error "Unknown execution method: %s" method)))))

(defun hybrid--execute-async (command script)
  "Execute command asynchronously with monitoring."
  (let* ((process-name (format "hybrid-script-%s" (hybrid-script-name script)))
         (buffer-name (format "*%s*" process-name))
         (process (start-process-shell-command process-name buffer-name command)))
    
    (set-process-sentinel process 'hybrid--process-sentinel)
    (hybrid--track-process process script)
    process))

(defun hybrid--execute-sync (command script)
  "Execute command synchronously with progress indication."
  (with-temp-buffer
    (let ((start-time (current-time))
          (exit-code (call-process-shell-command command nil t)))
      (let ((duration (float-time (time-subtract (current-time) start-time)))
            (output (buffer-string)))
        (hybrid--record-execution-result script exit-code duration output)
        (when (zerop exit-code)
          (message "✓ %s completed in %.2fs" (hybrid-script-name script) duration))
        (unless (zerop exit-code)
          (error "Script failed with exit code %d" exit-code))))))

;; === INTELLIGENT FEATURES ===

(defun hybrid--suggest-scripts (context)
  "Suggest relevant scripts based on context."
  (let ((scripts (hybrid--get-scripts-cached))
        (suggestions '()))
    
    ;; Simple context-based suggestions
    (dolist (script scripts)
      (let ((score (hybrid--calculate-relevance-score script context)))
        (when (> score 0.5)
          (push (cons script score) suggestions))))
    
    ;; Sort by relevance score
    (mapcar #'car (sort suggestions (lambda (a b) (> (cdr a) (cdr b)))))))

(defun hybrid--calculate-relevance-score (script context)
  "Calculate relevance score for script given context."
  (let ((score 0.0)
        (name (hybrid-script-name script))
        (desc (hybrid-script-description script))
        (tags (hybrid-script-tags script)))
    
    ;; Name matching
    (when (string-match-p context name)
      (setq score (+ score 0.8)))
    
    ;; Description matching
    (when (and desc (string-match-p context desc))
      (setq score (+ score 0.6)))
    
    ;; Tag matching
    (when (and tags (seq-some (lambda (tag) (string-match-p context tag)) tags))
      (setq score (+ score 0.4)))
    
    score))

;; === HELP SYSTEM (from Stallman approach) ===

(defun hybrid--show-script-help (script)
  "Show comprehensive help for script."
  (with-help-window "*Script Help*"
    (princ (format "SCRIPT: %s\n" (hybrid-script-name script)))
    (princ (make-string 50 ?=))
    (princ "\n\n")
    
    (when-let ((desc (hybrid-script-description script)))
      (princ (format "Description:\n%s\n\n" desc)))
    
    (princ (format "Path: %s\n" (hybrid-script-path script)))
    (princ (format "Size: %s\n" (hybrid--format-file-size (hybrid-script-size script))))
    (princ (format "Modified: %s\n\n" 
                   (format-time-string "%Y-%m-%d %H:%M:%S" 
                                     (hybrid-script-last-modified script))))
    
    (let ((args (hybrid-script-args script)))
      (when args
        (princ "Arguments:\n")
        (dolist (arg args)
          (princ (format "  --%s: %s\n" 
                        (plist-get arg :name)
                        (plist-get arg :description))))
        (princ "\n")))
    
    (when-let ((tags (hybrid-script-tags script)))
      (princ (format "Tags: %s\n\n" (string-join tags ", "))))
    
    (when-let ((help (hybrid-script-help script)))
      (princ (format "Help:\n%s\n" help)))))

;; === MAIN INTERFACE ===

(transient-define-prefix hybrid-scripts-menu ()
  "Hybrid script management interface"
  :info-manual "(hybrid-scripts) Top"
  :man-page "bash"
  [["Quick Actions"
    ("r" "Run script" hybrid-run-script)
    ("f" "Find & run" hybrid-find-and-run)
    ("l" "List all scripts" hybrid-list-scripts)
    ("s" "Search scripts" hybrid-search-scripts)]
   ["Script Management"
    ("e" "Edit script" hybrid-edit-script)
    ("n" "New script" hybrid-new-script)
    ("d" "Delete script" hybrid-delete-script)
    ("c" "Copy script" hybrid-copy-script)]
   ["Discovery & Organization"
    ("D" "Discover scripts" hybrid-discover-scripts)
    ("T" "Browse by tags" hybrid-browse-by-tags)
    ("R" "Recent scripts" hybrid-recent-scripts)
    ("F" "Favorites" hybrid-favorites)]]
  [["Advanced"
    ("b" "Batch operations" hybrid-batch-operations)
    ("p" "Performance monitor" hybrid-performance-monitor)
    ("L" "View logs" hybrid-view-logs)
    ("S" "Script dependencies" hybrid-script-dependencies)]
   ["Configuration"
    ("C" "Settings" hybrid-settings)
    ("H" "Help system" hybrid-help-system)
    ("?" "Show tutorial" hybrid-tutorial)]
   ["Exit"
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (unless hybrid--script-cache
    (hybrid--discover-scripts-with-caching)))

;; === PUBLIC API FUNCTIONS ===

(defun hybrid-run-script ()
  "Select and run a script interactively."
  (interactive)
  (let* ((scripts (hybrid--get-scripts-cached))
         (choices (mapcar (lambda (s) 
                           (cons (hybrid--format-script-info s) s)) 
                         scripts))
         (choice (completing-read "Run script: " choices))
         (script (alist-get choice choices nil nil #'string=)))
    (when script
      (hybrid--execute-script script))))

(defun hybrid-find-and-run ()
  "Intelligent script finder and runner."
  (interactive)
  (let* ((context (read-string "What do you want to do? "))
         (suggestions (hybrid--suggest-scripts context)))
    (if suggestions
        (let* ((choices (mapcar (lambda (s) 
                                 (cons (hybrid--format-script-info s) s)) 
                               suggestions))
               (choice (completing-read "Suggested scripts: " choices))
               (script (alist-get choice choices nil nil #'string=)))
          (when script
            (hybrid--execute-script script)))
      (message "No relevant scripts found for: %s" context))))

(defun hybrid-list-scripts ()
  "List all discovered scripts with details."
  (interactive)
  (let ((scripts (hybrid--get-scripts-cached)))
    (with-output-to-temp-buffer "*Script List*"
      (princ (format "DISCOVERED SCRIPTS (%d total)\n" (length scripts)))
      (princ (make-string 50 ?=))
      (princ "\n\n")
      
      (let ((groups (hybrid--group-scripts-by-tags scripts)))
        (dolist (group groups)
          (let ((tag (car group))
                (group-scripts (cdr group)))
            (princ (format "\n📁 %s (%d scripts)\n" tag (length group-scripts)))
            (princ (make-string 30 ?-))
            (princ "\n")
            (dolist (script group-scripts)
              (princ (format "  📄 %s\n" (hybrid--format-script-info script))))))))))

;;;###autoload
(defun hybrid-scripts ()
  "Launch hybrid script management system."
  (interactive)
  (hybrid-scripts-menu))

;; === UTILITY FUNCTIONS ===

(defun hybrid--format-file-size (size)
  "Format file size in human readable format."
  (cond
   ((> size 1048576) (format "%.1fM" (/ size 1048576.0)))
   ((> size 1024) (format "%.1fK" (/ size 1024.0)))
   (t (format "%dB" size))))

(defun hybrid--deduplicate-and-sort (scripts)
  "Remove duplicates and sort scripts."
  (let ((seen-paths (make-hash-table :test 'equal))
        (unique-scripts '()))
    
    (dolist (script scripts)
      (let ((path (hybrid-script-path script)))
        (unless (gethash path seen-paths)
          (puthash path t seen-paths)
          (push script unique-scripts))))
    
    (sort unique-scripts (lambda (a b) 
                          (string< (hybrid-script-name a) 
                                  (hybrid-script-name b))))))

;; Initialize on load
(when hybrid-scripts-auto-discovery
  (run-with-idle-timer 2 nil 'hybrid--discover-scripts-with-caching))

(provide 'hybrid-implementation)

;;; hybrid-implementation.el ends here