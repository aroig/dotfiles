#!/bin/bash
set -o errexit

arg="$1"
lib_path=$HOME/lib

case $1 in 
    start)       
        calibre --with-library="$lib_path"
        ;;
    

    stop)
        calibre --shutdown-running-calibre
        ;;


    edit)
        (
            cd "$lib_path"
            git annex edit metadata.db
        )
        ;;

    commit)
        (
            cd "$lib_path"
            git annex add metadata.db

            # if there are changed, commit
            num=$(git status --porcelain | wc -l)
            if [[ $num -ge 1 ]]; then
                git annex add .
                git add -A .
                git commit -m "Update library ($num files)"
            fi
        )
        ;;

esac
