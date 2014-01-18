#!/bin/bash
set -o errexit

arg="$1"
mail_path=$HOME/mail

case $1 in 
    start)       
        emacs -mail
        ;;
    

    stop)
        emacsclient -s mail -e '(progn (save-some-buffers t) (kill-emacs))'
        ;;


    commit)
        (
            cd "$mail_path"

            # if there are changes, commit
            num=$(git status --porcelain | wc -l)
            if [[ $num -ge 1 ]]; then                
                # add files
                git add -A .
                
                # recompute the number of changes and commit
                num=$(git status --porcelain | wc -l)
                git commit -m "auto-commit: new messages ($num files)"
            fi
        )
        ;;

esac
