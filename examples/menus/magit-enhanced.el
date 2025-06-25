;;; magit-enhanced.el --- Advanced script management with Magit-style interface

(require 'transient)
(require 'json)

(defvar me-config-file "~/.config/script-menus.json"
  "Configuration file for script menus.")

(defvar me-templates-dir "~/.config/script-templates"
  "Directory for script templates.")

(defstruct me-script
  name path description args help tags)

(defstruct me-menu
  name description scripts submenus)

(defvar me--cache nil "Cache for loaded menus.")

(defun me--load-config ()
  "Load configuration from JSON file."
  (when (file-exists-p me-config-file)
    (with-temp-buffer
      (insert-file-contents me-config-file)
      (json-read))))

(defun me--save-config (config)
  "Save configuration to JSON file."
  (with-temp-file me-config-file
    (insert (json-encode config))))

(defun me--find-scripts-recursive (dir)
  "Find all executable scripts recursively."
  (let ((scripts '()))
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively dir ".*" nil t))
        (when (and (file-executable-p file)
                   (file-regular-p file))
          (push (make-me-script 
                 :name (file-name-nondirectory file)
                 :path file
                 :description (me--extract-description file)
                 :args (me--extract-args file)
                 :help (me--extract-help file)
                 :tags (me--extract-tags file))
                scripts))))
    scripts))

(defun me--extract-description (file)
  "Extract description from script file."
  (with-temp-buffer
    (insert-file-contents file nil 0 500)
    (goto-char (point-min))
    (when (re-search-forward "^#\\s-*Description:\\s-*\\(.*\\)$" nil t)
      (match-string 1))))

(defun me--extract-args (file)
  "Extract argument definitions from script."
  (with-temp-buffer
    (insert-file-contents file nil 0 1000)
    (let ((args '()))
      (goto-char (point-min))
      (while (re-search-forward "^#\\s-*@arg\\s-+\\([^:]+\\):\\s-*\\(.*\\)$" nil t)
        (push (cons (match-string 1) (match-string 2)) args))
      (nreverse args))))

(defun me--extract-help (file)
  "Extract help text from script."
  (with-temp-buffer
    (insert-file-contents file nil 0 2000)
    (goto-char (point-min))
    (when (re-search-forward "^#\\s-*Help:\\s-*\\(\\(?:.*\n\\)*?\\)^#\\s-*$" nil t)
      (match-string 1))))

(defun me--extract-tags (file)
  "Extract tags from script."
  (with-temp-buffer
    (insert-file-contents file nil 0 500)
    (goto-char (point-min))
    (when (re-search-forward "^#\\s-*Tags:\\s-*\\(.*\\)$" nil t)
      (split-string (match-string 1) "[, ]+" t))))

(defun me--build-dynamic-menu (menu-name scripts &optional level)
  "Build dynamic transient menu from scripts."
  (let ((level (or level 0))
        (groups (me--group-scripts scripts))
        (sections '()))
    
    (dolist (group groups)
      (let ((group-name (car group))
            (group-scripts (cdr group))
            (keys "abcdefghijklmnopqrstuvwxyz"))
        (push 
         (vconcat 
          (list (format "%s Scripts" group-name))
          (mapcar (lambda (script)
                    (let ((key (string (aref keys (length section))))
                          (name (me-script-name script))
                          (desc (or (me-script-description script) name)))
                      (list key desc `(lambda () (interactive) 
                                        (me--execute-script ,script)))))
                  group-scripts))
         sections)))
    
    (push (vector "Actions"
                  '("r" "refresh" me--refresh-menu)
                  '("e" "edit script" me--edit-script)
                  '("n" "new script" me--new-script)
                  '("h" "help" me--show-help))
          sections)
    
    (push (vector "Navigation"
                  '("q" "quit" transient-quit-one)
                  (when (> level 0) '("b" "back" me--go-back)))
          sections)
    
    sections))

(defun me--group-scripts (scripts)
  "Group scripts by tags or directory."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (script scripts)
      (let ((tags (me-script-tags script)))
        (if tags
            (dolist (tag tags)
              (puthash tag (cons script (gethash tag groups)) groups))
          (puthash "Other" (cons script (gethash "Other" groups)) groups))))
    (hash-table-alist groups)))

(defun me--execute-script (script &optional args)
  "Execute script with dynamic argument collection."
  (let ((script-args (me-script-args script))
        (collected-args '()))
    
    (when script-args
      (dolist (arg script-args)
        (let* ((arg-name (car arg))
               (arg-desc (cdr arg))
               (value (read-string (format "%s (%s): " arg-name arg-desc))))
          (push (format "--%s=%s" arg-name value) collected-args))))
    
    (let ((command (format "%s %s" 
                          (me-script-path script)
                          (string-join (nreverse collected-args) " "))))
      (async-shell-command command))))

(defun me--show-script-help (script)
  "Show detailed help for script."
  (with-help-window "*Script Help*"
    (princ (format "Script: %s\n" (me-script-name script)))
    (princ (format "Path: %s\n" (me-script-path script)))
    (when (me-script-description script)
      (princ (format "\nDescription:\n%s\n" (me-script-description script))))
    (when (me-script-args script)
      (princ "\nArguments:\n")
      (dolist (arg (me-script-args script))
        (princ (format "  --%s: %s\n" (car arg) (cdr arg)))))
    (when (me-script-help script)
      (princ (format "\nHelp:\n%s\n" (me-script-help script))))
    (when (me-script-tags script)
      (princ (format "\nTags: %s\n" (string-join (me-script-tags script) ", "))))))

(transient-define-prefix me-main-menu ()
  "Enhanced script management interface"
  :info-manual "(magit-enhanced) Top"
  :man-page "bash"
  :setup-children me--setup-dynamic-menu
  [["Quick Actions"
    ("r" "run script" me-run-script)
    ("e" "edit script" me-edit-script)
    ("n" "new from template" me-new-from-template)]
   ["Discovery"
    ("f" "find scripts" me-find-scripts)
    ("s" "search by tag" me-search-tags)
    ("/" "filter scripts" me-filter-scripts)]
   ["Management"
    ("c" "configure menus" me-configure-menus)
    ("t" "manage templates" me-manage-templates)
    ("R" "refresh cache" me-refresh-cache)]]
  [["Help & Info"
    ("h" "help" me-show-help)
    ("?" "script help" me-show-script-help-interactive)
    ("i" "info" me-show-info)]
   ["Exit"
    ("q" "quit" transient-quit-one)]])

(defun me--setup-dynamic-menu (_)
  "Setup dynamic menu children based on discovered scripts."
  (let ((scripts (me--find-scripts-recursive "~/scripts")))
    (me--build-dynamic-menu "Main" scripts)))

(defun me-run-script ()
  "Interactively run a script."
  (interactive)
  (let* ((scripts (me--find-scripts-recursive "~/scripts"))
         (choices (mapcar (lambda (s) 
                          (cons (format "%s - %s" 
                                       (me-script-name s)
                                       (or (me-script-description s) "")) 
                                s)) 
                         scripts))
         (choice (completing-read "Run script: " choices))
         (script (alist-get choice choices nil nil #'string=)))
    (when script
      (me--execute-script script))))

(defun me-edit-script ()
  "Edit a script."
  (interactive)
  (let* ((scripts (me--find-scripts-recursive "~/scripts"))
         (choices (mapcar (lambda (s) (cons (me-script-name s) s)) scripts))
         (choice (completing-read "Edit script: " choices))
         (script (alist-get choice choices nil nil #'string=)))
    (when script
      (find-file (me-script-path script)))))

(defun me-new-from-template ()
  "Create new script from template."
  (interactive)
  (let* ((templates (directory-files me-templates-dir nil "\\.template$"))
         (template (completing-read "Template: " templates))
         (name (read-string "Script name: "))
         (template-path (expand-file-name template me-templates-dir))
         (script-path (expand-file-name name "~/scripts")))
    (copy-file template-path script-path)
    (find-file script-path)
    (me--substitute-template-vars name)))

(defun me--substitute-template-vars (script-name)
  "Substitute template variables in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "{{\\([^}]+\\)}}" nil t)
      (let ((var (match-string 1)))
        (replace-match 
         (cond
          ((string= var "NAME") script-name)
          ((string= var "DATE") (format-time-string "%Y-%m-%d"))
          ((string= var "AUTHOR") user-full-name)
          (t "")) 
         t t)))))

;;;###autoload
(defun magit-enhanced-scripts ()
  "Launch enhanced script management interface."
  (interactive)
  (me-main-menu))

(provide 'magit-enhanced)