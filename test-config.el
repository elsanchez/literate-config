(condition-case err
    (progn
      (load "~/.config/doom/config.el")
      (message "✅ Config loaded successfully"))
  (error (message "❌ Error: %s" err)))