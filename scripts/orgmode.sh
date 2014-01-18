#!/bin/bash
set -o errexit

arg="$1"
wiki_path=$HOME/work/wiki

case $1 in 
    start)       
        emacs -org
        ;;
    

    stop)
        emacsclient -s org -e '(progn (save-some-buffers t) (kill-emacs))'
        ;;


    commit)
        (
            cd "$wiki_path"

            # if there are changes, commit
            num=$(git status --porcelain | wc -l)
            if [[ $num -ge 1 ]]; then                
                # add files
                git add -A .
                
                # recompute the number of changes and commit
                num=$(git status --porcelain | wc -l)
                git commit -m "auto-commit: modified content ($num files)"
            fi
        )
        ;;

esac
