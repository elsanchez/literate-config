#!/bin/bash
# Description: Clean up merged git branches
# Tags: git, cleanup
# @arg dry-run: Show what would be deleted (true/false)

if [ "$1" = "true" ]; then
  echo "DRY RUN: Would delete:"
  git branch --merged | grep -v master | grep -v main
else
  echo "Deleting merged branches..."
  git branch --merged | grep -v master | grep -v main | xargs -n 1 git branch -d
fi