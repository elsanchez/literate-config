#!/bin/bash
# Quick test of a specific implementation

IMPL=${1:-linus}
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case $IMPL in
    linus|l)
        echo "🔧 Testing Linus implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-individual \"linus\")"
        ;;
    stallman|s)
        echo "📚 Testing Stallman implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-individual \"stallman\")"
        ;;
    magit|m)
        echo "⚡ Testing Magit Enhanced implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-individual \"magit\")"
        ;;
    hybrid|h)
        echo "🧬 Testing Hybrid implementation..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-hybrid)"
        ;;
    basic|b)
        echo "🧪 Testing Basic framework..."
        emacs --batch \
              --load "$BASE_DIR/simple-loader.el" \
              --eval "(simple-loader-load-basic)"
        ;;
    python|p)
        echo "🐍 Testing Python implementation..."
        PYTHON_SCRIPT="$BASE_DIR/../examples/menus/script_runner.py"
        if [ -f "$PYTHON_SCRIPT" ]; then
            python3 "$PYTHON_SCRIPT" --config "$BASE_DIR/configs/test-runner.yaml"
        else
            echo "❌ Python script not found"
        fi
        ;;
    *)
        echo "Usage: $0 [linus|stallman|magit|hybrid|basic|python]"
        echo ""
        echo "Available implementations:"
        echo "  linus     - Linus Torvalds style (fast, pragmatic)"
        echo "  stallman  - Richard Stallman style (comprehensive)"
        echo "  magit     - Magit Enhanced style (visual)"
        echo "  hybrid    - Hybrid implementation (best UX)"
        echo "  basic     - Basic test framework"
        echo "  python    - Python TUI version"
        exit 1
        ;;
esac
