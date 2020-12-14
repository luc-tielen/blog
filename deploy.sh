#!/bin/bash

# Run this script locally to trigger a deploy.

set -e

echo "Generating blog..."
stack run

echo "Deploying blog..."
git checkout deploy
git add build/
git commit -m 'Deploy new version'
git push
git checkout -

exit 0

