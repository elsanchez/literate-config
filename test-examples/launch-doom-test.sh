#!/bin/bash
# Launch Doom Emacs with test profile

if ! command -v doom &> /dev/null; then
    echo "❌ Doom Emacs not found. Please install Doom first."
    exit 1
fi

echo "🧪 Launching Doom Emacs with test profile..."
echo "📁 Profile: /home/elsanchez/.config/doom-test"
echo "🎯 Press SPC t for test menu"
echo ""

# Set DOOMDIR and launch
export DOOMDIR="/home/elsanchez/.config/doom-test"
emacs "$@"
