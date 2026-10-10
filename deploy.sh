#!/usr/bin/env bash

# Build the Astro site and publish it to the deploy branch watched by Netlify.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
deploy_worktree="$(mktemp -d)"

cleanup() {
  git -C "$repo_root" worktree remove --force "$deploy_worktree" >/dev/null 2>&1 || true
  rm -rf "$deploy_worktree"
}
trap cleanup EXIT

cd "$repo_root"

if [[ -n "$(git status --porcelain)" ]]; then
  echo "The working tree must be clean before deploying." >&2
  exit 1
fi

echo "Building Astro site..."
npm ci
npm run build

echo "Preparing deploy branch..."
git fetch origin deploy
git worktree add --detach "$deploy_worktree" origin/deploy

rm -rf "$deploy_worktree/build"
mkdir -p "$deploy_worktree/build"
cp -R "$repo_root/dist/." "$deploy_worktree/build/"

git -C "$deploy_worktree" add --all build

if git -C "$deploy_worktree" diff --cached --quiet; then
  echo "No deployment changes found."
  exit 0
fi

git -C "$deploy_worktree" commit -m "Deploy new version"
git -C "$deploy_worktree" push origin HEAD:deploy

echo "Published Astro build to the deploy branch for Netlify."
