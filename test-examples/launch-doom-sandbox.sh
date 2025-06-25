#!/bin/bash
# Launch Doom Sandbox for Script Menu Testing

echo "🏖️ Launching Doom Sandbox..."
echo "📁 Config: /home/elsanchez/.config/doom-sandbox"
echo "🎯 Press SPC t t for main test menu"
echo ""

# Set environment variables
export DOOMDIR="/home/elsanchez/.config/doom-sandbox"
export DOOMPROFILE="sandbox"

# Launch Emacs with sandbox config
emacs --init-directory="/home/elsanchez/.config/doom-sandbox" "$@"
