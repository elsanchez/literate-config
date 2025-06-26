#!/bin/bash
# Description: Build application
# Tags: build, ci-cd
# @arg target: Build target (dev/prod)
# @arg clean: Clean before build (true/false)

if [ "$2" = "true" ]; then
  echo "Cleaning previous build..."
fi
echo "Building for $1..."
sleep 2
echo "✓ Build completed"