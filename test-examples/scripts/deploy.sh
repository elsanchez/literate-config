#!/bin/bash
# Description: Deploy application to target environment
# Tags: deployment, ci-cd
# @arg environment: Target environment (dev/staging/prod)
# @arg version: Version to deploy (optional)

echo "Deploying to $1 with version ${2:-latest}"
sleep 2
echo "✓ Deployment completed"