#!/bin/bash
# Launch Emacs with test profile

echo "🧪 Launching Emacs with test profile..."
echo "📁 Profile: /home/elsanchez/.config/emacs-test-profile"
echo "🎯 Press C-c t for test menu"
echo ""

emacs --init-directory="/home/elsanchez/.config/emacs-test-profile" "$@"
