#!/bin/sh

# ref https://gist.github.com/martinbuberl/b58fd967f271f32f51f50aee62e7332c

# This script takes a remote repository and merges it into
# the current one as a subdirectory

set -e

if [ -z "$1" ]
then
    echo "Usage:"
    echo "    ./merge_repos.sh <repository> [name]"
    echo "        <repository> remote repository to merge"
    echo "        [name]       sub-directory name (optional)"
    exit
fi

REPO_REMOTE="$1"
REPO_NAME="$2"

# infer a name if one is not provided
if [ -z "$REPO_NAME" ]
then
    REPO_NAME="${REPO_REMOTE##*/}"
    REPO_NAME="${REPO_NAME%.*}"
fi

git remote add "$REPO_NAME" "$REPO_REMOTE"
git fetch "$REPO_NAME" master

git merge --strategy=ours --no-commit --allow-unrelated-histories "$REPO_NAME/master"
git read-tree --prefix="$REPO_NAME/" -u "$REPO_NAME/master"

git commit --message="chore: import $REPO_NAME"

git remote rm "$REPO_NAME"
