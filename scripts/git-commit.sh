#!/bin/bash
set -o errexit

# Commit a git repo at the given path
repo="$1"

cd "$repo"

# stop if it is not a git repo
[ ! -d ".git" ] && exit 0

# check for changes
num=$(git status --porcelain | wc -l)    
if [[ $num -ge 1 ]]; then            
    echo "adding files to '$repo'"    
    [ -d ".git/annex" ] && git annex add .
    git add -A .
                
    # recompute the number of changes and commit
    num=$(git status --porcelain | wc -l)
    echo "commiting files to '$repo'"            
    git commit -q -m "auto-commit ($num files)"
fi


