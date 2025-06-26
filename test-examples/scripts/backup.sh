#!/bin/bash
# Description: Backup important files
# Tags: backup, maintenance
# @arg destination: Backup destination path
# @arg compress: Compress backup (true/false)

echo "Creating backup to $1..."
if [ "$2" = "true" ]; then
  echo "Using compression..."
fi
sleep 1
echo "✓ Backup completed"