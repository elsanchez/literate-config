;;; linustorv.el --- Script runner that doesn't completely suck

;; Look, I don't know why you need a fancy GUI to run a damn shell script,
;; but here we are. At least this doesn't suck as much as systemd.

(require 'transient)

(defvar ts-script-dir "~/bin" 
  "Where your scripts live. ~/bin, like a civilized human.")

(defvar ts-shell "/bin/bash"
  "Default shell. Bash, because zsh users are hipsters.")

(defvar ts-async t
  "Run async because blocking is for Windows users.")

(defvar ts--history nil)

(defun ts--get-scripts ()
  "Find executable files like we're not idiots."
  (when (file-directory-p ts-script-dir)
    (seq-filter (lambda (f) 
                  (and (not (string-prefix-p "." f))
                       (file-executable-p (expand-file-name f ts-script-dir))))
                (directory-files ts-script-dir))))

(defun ts--run (script &optional args)
  "Execute SCRIPT. Revolutionary concept, I know."
  (let ((cmd (format "%s/%s %s" ts-script-dir script (or args ""))))
    (if ts-async
        (start-process-shell-command "ts" nil cmd)
      (shell-command cmd))))

(defun ts-run-script ()
  "Pick a script and run it. Groundbreaking."
  (interactive)
  (let ((script (completing-read "Script: " (ts--get-scripts) nil t nil 'ts--history)))
    (ts--run script)))

(defun ts-edit-script ()
  "Edit a script. Mind = blown."
  (interactive)
  (find-file (expand-file-name 
              (completing-read "Edit: " (ts--get-scripts)) 
              ts-script-dir)))

(defun ts-new-script (name)
  "Create new script. Don't make it suck."
  (interactive "sName: ")
  (let ((file (expand-file-name name ts-script-dir)))
    (with-temp-file file
      (insert "#!/bin/bash\n# TODO: Make this not suck\n\n"))
    (set-file-modes file #o755)
    (find-file file)))

;; Magit-style interface because apparently people need pretty colors
(transient-define-prefix ts-menu ()
  "Script management for people who can't use ls"
  :man-page "bash"
  :info-manual "(bash) Top"
  [["Quick Actions"
    ("r" "run script" ts-run-script :transient t)
    ("e" "edit script" ts-edit-script)
    ("n" "new script" ts-new-script)]
   ["Directory Operations"
    ("d" "open directory" (lambda () (interactive) (dired ts-script-dir)))
    ("f" "find file" (lambda () (interactive) (find-file ts-script-dir)))
    ("s" "shell here" (lambda () (interactive) 
                        (let ((default-directory ts-script-dir))
                          (shell))))]
   ["Git Operations"
    ("g" "git menu" ts-git-menu)]]
  [["Settings"
    ("C" "change directory" ts-change-dir)
    ("S" "toggle async" ts-toggle-async)]
   ["Help & Info"
    ("h" "help" ts-help)
    ("?" "show keybindings" ts-show-keys)]
   ["Exit"
    ("q" "quit" transient-quit-one)]]
  (interactive)
  (unless (file-directory-p ts-script-dir)
    (make-directory ts-script-dir t)
    (message "Created %s because you didn't have it" ts-script-dir)))

(transient-define-prefix ts-git-menu ()
  "Git operations submenu"
  :man-page "git"
  [["Status & Info"
    ("s" "status" (lambda () (interactive) (ts--run "git-status")))
    ("l" "log" (lambda () (interactive) (ts--run "git-log")))
    ("d" "diff" (lambda () (interactive) (ts--run "git-diff")))
    ("b" "branches" (lambda () (interactive) (ts--run "git-branches")))]
   ["Operations"
    ("c" "commit" (lambda () (interactive) (ts--run "git-commit")))
    ("p" "push" (lambda () (interactive) (ts--run "git-push")))
    ("P" "pull" (lambda () (interactive) (ts--run "git-pull")))
    ("m" "merge" (lambda () (interactive) (ts--run "git-merge")))]
   ["Advanced"
    ("r" "rebase" (lambda () (interactive) (ts--run "git-rebase")))
    ("C" "cherry-pick" (lambda () (interactive) (ts--run "git-cherry")))
    ("R" "reset" (lambda () (interactive) (ts--run "git-reset")))]]
  [["Back"
    ("B" "back to main" ts-menu)
    ("q" "quit" transient-quit-one)]])

(defun ts-change-dir ()
  "Change script directory."
  (interactive)
  (setq ts-script-dir (read-directory-name "Script directory: " ts-script-dir))
  (message "Script directory: %s" ts-script-dir))

(defun ts-toggle-async ()
  "Toggle async execution."
  (interactive)
  (setq ts-async (not ts-async))
  (message "Async execution: %s" (if ts-async "enabled" "disabled")))

(defun ts-help ()
  "Show help."
  (interactive)
  (with-help-window "*TS Help*"
    (princ "Linus Torvalds Script Runner\n\n")
    (princ "Basic usage:\n")
    (princ "  r - Run a script (duh)\n")
    (princ "  e - Edit a script\n")
    (princ "  n - Create new script\n")
    (princ "  d - Open script directory\n\n")
    (princ "Configuration:\n")
    (princ (format "  Script directory: %s\n" ts-script-dir))
    (princ (format "  Async execution: %s\n" ts-async))
    (princ (format "  Shell: %s\n" ts-shell))))

(defun ts-show-keys ()
  "Show keybindings."
  (interactive)
  (describe-function 'ts-menu))

;;;###autoload
(defun linus-scripts ()
  "Launch the script thing. Try not to break anything."
  (interactive)
  (ts-menu))

(provide 'linustorv)

;; Look, this is pretty simple:
;; 1. Put your scripts in ~/bin
;; 2. Make them executable (chmod +x, genius)
;; 3. Press M-x linus-scripts
;; 4. Stop bothering me
;;
;; If this doesn't work, the problem is probably between
;; your chair and your keyboard.