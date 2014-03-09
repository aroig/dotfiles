#!/bin/bash
set -o errexit

arg="$1"
dwb_path=$HOME/priv/var/dwb/default

case $1 in 
    start)       
        exec dwb
        ;;
    

    stop)
        pkill dwb || true
        ;;


    commit)
        (
            cd "$dwb_path"

            # if there are changes, commit
            num=$(git status --porcelain | wc -l)
            if [[ $num -ge 1 ]]; then                
                # add files
                git add -A .
                
                # recompute the number of changes and commit
                num=$(git status --porcelain | wc -l)
                git commit -m "auto-commit: files changed ($num files)" > /dev/null
            fi
        )
        ;;

esac
