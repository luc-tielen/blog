#!/bin/bash

# Run this script locally to trigger a deploy.

set -e

echo "Generating blog..."
stack run
tar czf build.tar.gz build/

echo "Updating blog content..."
git checkout deploy
rm -rf build/
tar xzf build.tar.gz
rm build.tar.gz

echo "Deploying blog..."
git add build/
git commit -m 'Deploy new version'
git push

git checkout -

exit 0

