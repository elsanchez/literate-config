#!/bin/bash
# Quick test of a specific implementation

IMPL=${1:-linus}

case $IMPL in
    linus|l)
        echo "🔧 Testing Linus implementation..."
        emacs --batch --load ~/org/literate-config/test-examples/test-runner.el --eval "(test-linus-scripts)"
        ;;
    stallman|s)
        echo "📚 Testing Stallman implementation..."
        emacs --batch --load ~/org/literate-config/test-examples/test-runner.el --eval "(test-stallman-scripts)"
        ;;
    magit|m)
        echo "⚡ Testing Magit Enhanced implementation..."
        emacs --batch --load ~/org/literate-config/test-examples/test-runner.el --eval "(test-magit-enhanced-scripts)"
        ;;
    python|p)
        echo "🐍 Testing Python implementation..."
        ~/org/literate-config/examples/menus/script_runner.py --config ~/org/literate-config/test-examples/configs/test-runner.yaml
        ;;
    *)
        echo "Usage: $0 [linus|stallman|magit|python]"
        exit 1
        ;;
esac
