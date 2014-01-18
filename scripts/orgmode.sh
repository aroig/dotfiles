#!/bin/bash
set -o errexit

arg="$1"
wiki_path=$HOME/work/wiki

case $1 in 
    start)       
        emacs -org --title "emacs-org"
        ;;
    

    stop)
        # stop emacs saving buffers if the process is alive, otherwise fail quietly.
        emacsclient -s org -e '(progn (save-some-buffers t) (kill-emacs))' 2> /dev/null || true
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
