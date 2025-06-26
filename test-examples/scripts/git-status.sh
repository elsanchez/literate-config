#!/bin/bash
# Description: Enhanced git status
# Tags: git, status

echo "=== Git Status ==="
git status --porcelain
echo ""
echo "=== Branch Info ==="
git branch -v