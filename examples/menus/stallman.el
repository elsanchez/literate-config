;;; stallman.el --- Transient interface for script execution

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Richard Matthew Stallman <rms@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Keywords: tools, processes, unix, freedom
;; URL: https://www.gnu.org/software/emacs/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a transient interface for managing and executing
;; shell scripts.  It embodies the GNU philosophy that users should have
;; complete control over their computing environment.
;;
;; Freedom to compute includes freedom to automate repetitive tasks through
;; scripts.  This interface makes script management accessible while
;; preserving the user's freedom to modify and extend functionality.

;;; Code:

(require 'transient)

(defgroup stallman-scripts nil
  "Script execution interface respecting user freedom."
  :group 'applications
  :group 'processes
  :prefix "stallman-scripts-")

(defcustom stallman-scripts-directory "~/scripts"
  "Directory containing user scripts.
This directory represents the user's freedom to organize their
automation tools as they see fit."
  :type 'directory
  :group 'stallman-scripts)

(defcustom stallman-scripts-shell "/bin/bash"
  "Shell interpreter for script execution.
Users should be free to choose their preferred shell."
  :type 'string
  :group 'stallman-scripts)

(defcustom stallman-scripts-async t
  "Whether to execute scripts asynchronously.
Asynchronous execution preserves the user's freedom to continue
working while scripts run."
  :type 'boolean
  :group 'stallman-scripts)

(defcustom stallman-scripts-confirm-execution nil
  "Whether to confirm before executing scripts.
Confirmation prevents accidental execution while respecting
user autonomy."
  :type 'boolean
  :group 'stallman-scripts)

(defvar stallman-scripts-history nil
  "History of executed scripts.")

(defvar stallman-scripts-favorites nil
  "List of frequently used scripts.")

(defun stallman-scripts--find-executable-files ()
  "Locate executable files in the scripts directory.
This function respects the user's file organization choices."
  (when (file-directory-p stallman-scripts-directory)
    (seq-filter (lambda (filename)
                  (let ((full-path (expand-file-name filename stallman-scripts-directory)))
                    (and (file-regular-p full-path)
                         (file-executable-p full-path)
                         (not (string-prefix-p "." filename)))))
                (directory-files stallman-scripts-directory))))

(defun stallman-scripts--execute-script (script-name &optional arguments)
  "Execute SCRIPT-NAME with optional ARGUMENTS.
This function provides the core functionality for script execution
while respecting user preferences for synchronous or asynchronous
operation."
  (let* ((script-path (expand-file-name script-name stallman-scripts-directory))
         (command (format "%s %s %s" 
                         stallman-scripts-shell 
                         (shell-quote-argument script-path)
                         (or arguments ""))))
    (when (or (not stallman-scripts-confirm-execution)
              (yes-or-no-p (format "Execute script '%s'? " script-name)))
      (add-to-list 'stallman-scripts-history script-name)
      (if stallman-scripts-async
          (async-shell-command command)
        (shell-command command)))))

(defun stallman-scripts-execute-script ()
  "Interactively select and execute a script.
This command demonstrates the principle that users should have
convenient access to their automation tools."
  (interactive)
  (let ((available-scripts (stallman-scripts--find-executable-files)))
    (if available-scripts
        (let ((selected-script (completing-read "Execute script: " 
                                               available-scripts 
                                               nil t nil 
                                               'stallman-scripts-history)))
          (stallman-scripts--execute-script selected-script))
      (message "No executable scripts found in %s" stallman-scripts-directory))))

(defun stallman-scripts-edit-script ()
  "Edit an existing script.
Editing capabilities are fundamental to user freedom in computing."
  (interactive)
  (let ((available-scripts (stallman-scripts--find-executable-files)))
    (if available-scripts
        (let ((selected-script (completing-read "Edit script: " available-scripts)))
          (find-file (expand-file-name selected-script stallman-scripts-directory)))
      (message "No scripts found to edit in %s" stallman-scripts-directory))))

(defun stallman-scripts-create-new-script (script-name)
  "Create a new script named SCRIPT-NAME.
This function empowers users to create new automation tools."
  (interactive "sScript name: ")
  (let ((script-path (expand-file-name script-name stallman-scripts-directory)))
    (unless (file-exists-p stallman-scripts-directory)
      (make-directory stallman-scripts-directory t))
    (find-file script-path)
    (when (zerop (buffer-size))
      (insert (format "#!%s\n" stallman-scripts-shell))
      (insert (format "# %s - Created by Stallman Scripts\n" script-name))
      (insert "# This script embodies your freedom to automate\n\n")
      (save-buffer))
    (set-file-modes script-path #o755)))

(defun stallman-scripts-browse-directory ()
  "Browse the scripts directory using Dired.
Directory browsing provides transparency about script organization."
  (interactive)
  (unless (file-exists-p stallman-scripts-directory)
    (make-directory stallman-scripts-directory t))
  (dired stallman-scripts-directory))

(defun stallman-scripts-show-configuration ()
  "Display current configuration settings.
Transparency about configuration respects user agency."
  (interactive)
  (with-output-to-temp-buffer "*Stallman Scripts Configuration*"
    (princ "Stallman Scripts Configuration\n")
    (princ "=============================\n\n")
    (princ (format "Scripts directory: %s\n" stallman-scripts-directory))
    (princ (format "Shell interpreter: %s\n" stallman-scripts-shell))
    (princ (format "Asynchronous execution: %s\n" 
                   (if stallman-scripts-async "enabled" "disabled")))
    (princ (format "Confirm execution: %s\n" 
                   (if stallman-scripts-confirm-execution "enabled" "disabled")))
    (princ (format "Available scripts: %d\n" 
                   (length (stallman-scripts--find-executable-files))))))

(transient-define-prefix stallman-scripts-menu ()
  "Main interface for script management.
This transient menu provides organized access to script operations
while maintaining the flexibility that users deserve."
  :man-page "bash"
  :info-manual "(bash) Top"
  [["Script Operations"
    ("e" "Execute script" stallman-scripts-execute-script)
    ("E" "Edit script" stallman-scripts-edit-script)
    ("n" "New script" stallman-scripts-create-new-script)]
   ["Directory Management"
    ("d" "Browse directory" stallman-scripts-browse-directory)
    ("f" "Find file in directory" 
     (lambda () (interactive) 
       (find-file (read-file-name "Find file: " stallman-scripts-directory))))
    ("s" "Shell in directory" stallman-scripts-shell-in-directory)]
   ["Version Control"
    ("g" "Git operations" stallman-scripts-git-menu)
    ("v" "View git log" stallman-scripts-git-log)]]
  [["Configuration"
    ("c" "Show configuration" stallman-scripts-show-configuration)
    ("C" "Customize settings" 
     (lambda () (interactive) (customize-group 'stallman-scripts)))
    ("t" "Toggle async execution" stallman-scripts-toggle-async)]
   ["Help & Information"
    ("h" "Help" stallman-scripts-help)
    ("?" "Show keybindings" describe-mode)
    ("i" "Info manual" stallman-scripts-info)]
   ["Exit"
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (unless (file-directory-p stallman-scripts-directory)
    (when (yes-or-no-p (format "Create scripts directory %s? " 
                              stallman-scripts-directory))
      (make-directory stallman-scripts-directory t))))

(transient-define-prefix stallman-scripts-git-menu ()
  "Git operations for script management.
Version control is essential for maintaining script history and
collaboration."
  [["Repository Status"
    ("s" "Git status" stallman-scripts-git-status)
    ("l" "Git log" stallman-scripts-git-log)
    ("d" "Git diff" stallman-scripts-git-diff)]
   ["Commit Operations"
    ("c" "Git commit" stallman-scripts-git-commit)
    ("a" "Git add all" stallman-scripts-git-add-all)
    ("p" "Git push" stallman-scripts-git-push)]
   ["Branch Management"
    ("b" "Git branch" stallman-scripts-git-branch)
    ("m" "Git merge" stallman-scripts-git-merge)
    ("r" "Git rebase" stallman-scripts-git-rebase)]]
  [["Return"
    ("B" "Back to main menu" stallman-scripts-menu)
    ("q" "Quit" transient-quit-one)]])

(defun stallman-scripts-shell-in-directory ()
  "Open a shell in the scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (shell)))

(defun stallman-scripts-toggle-async ()
  "Toggle asynchronous execution mode."
  (interactive)
  (setq stallman-scripts-async (not stallman-scripts-async))
  (message "Asynchronous execution %s" 
           (if stallman-scripts-async "enabled" "disabled")))

(defun stallman-scripts-help ()
  "Display comprehensive help information."
  (interactive)
  (with-help-window "*Stallman Scripts Help*"
    (princ "Stallman Scripts - Free Software Script Management\n")
    (princ "==============================================\n\n")
    (princ "This package embodies the GNU philosophy of user freedom\n")
    (princ "in computing. You have the freedom to:\n\n")
    (princ "- Run scripts (freedom 0)\n")
    (princ "- Study and modify this code (freedom 1)\n")
    (princ "- Redistribute copies (freedom 2)\n")
    (princ "- Distribute modified versions (freedom 3)\n\n")
    (princ "Basic Operations:\n")
    (princ "  e - Execute a script\n")
    (princ "  E - Edit a script\n")
    (princ "  n - Create new script\n")
    (princ "  d - Browse scripts directory\n\n")
    (princ "For more information, see the Info manual.\n")))

(defun stallman-scripts-info ()
  "Open the Info manual."
  (interactive)
  (info "(stallman-scripts) Top"))

;; Git operation functions
(defun stallman-scripts-git-status ()
  "Show git status for scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-status)))

(defun stallman-scripts-git-log ()
  "Show git log for scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-log-current)))

(defun stallman-scripts-git-diff ()
  "Show git diff for scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-diff-working-tree)))

(defun stallman-scripts-git-commit ()
  "Commit changes in scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-commit-create)))

(defun stallman-scripts-git-add-all ()
  "Add all changes in scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (shell-command "git add .")))

(defun stallman-scripts-git-push ()
  "Push changes in scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-push-current-to-upstream)))

(defun stallman-scripts-git-branch ()
  "Show git branches for scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-branch)))

(defun stallman-scripts-git-merge ()
  "Merge branches in scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-merge)))

(defun stallman-scripts-git-rebase ()
  "Rebase branches in scripts directory."
  (interactive)
  (let ((default-directory stallman-scripts-directory))
    (magit-rebase)))

;;;###autoload
(defun stallman-scripts ()
  "Launch the Stallman Scripts interface.
This command provides entry to a comprehensive script management
system that respects user freedom and promotes software libre
principles."
  (interactive)
  (stallman-scripts-menu))

(provide 'stallman)

;;; stallman.el ends here

;; Local Variables:
;; coding: utf-8
;; End: