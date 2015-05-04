#!/bin/bash
set -o errexit

# Commit a git repo at the given path

repo="$1"
cd "$repo"

# stop if it is not a git repo
if [ ! -d ".git" ]; then
    exit 0
fi

# check for changes
num=$(git status --porcelain | wc -l)    
if [[ $num -ge 1 ]]; then            
    echo "adding files to '$repo'"    
    [ -d ".git/annex" ] && git annex add . > /dev/null
    git add -A . > /dev/null
else
    exit 0
fi
                
# commit changes
host=$(hostname -s)
num=$(git status --porcelain | wc -l)
if [[ $num -ge 1 ]]; then            
    echo "commiting files to '$repo'"            
    git commit -q -m "auto-commit on $host ($num files)"
fi
