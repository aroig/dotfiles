#!/bin/bash
set -o errexit

arg="$1"
lib_path=$HOME/lib

case $1 in 
    start)       
        exec calibre --with-library="$lib_path"
        ;;
    

    stop)
        calibre --shutdown-running-calibre
        ;;


    unlock)
        (
            cd "$lib_path"
            git annex edit metadata.db
        )
        ;;

    commit)
        (
            cd "$lib_path"
            git annex add metadata.db

            # if there are changes, commit
            num=$(git status --porcelain | wc -l)
            if [[ $num -ge 1 ]]; then                
                # add files
                git annex add .
                git add -A .
                
                # recompute the number of changes and commit
                num=$(git status --porcelain | wc -l)
                git commit -m "auto-commit: update library ($num files)"
            fi
        )
        ;;

esac
