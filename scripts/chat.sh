#!/bin/bash
set -o errexit

arg="$1"
log_path=$HOME/var/chat/emacs

case $1 in 
    start)       
        emacs -chat --title "emacs-chat"
        ;;
    

    stop)
        # stop emacs saving buffers if the process is alive, otherwise fail quietly.
        emacsclient -s chat -e '(progn (abdo-chat-disconnect) (save-some-buffers t) (kill-emacs))' 2> /dev/null || true
        ;;


    commit)
        (
            cd "$log_path"

            # if there are changes, commit
            num=$(git status --porcelain | wc -l)
            if [[ $num -ge 1 ]]; then                
                # add files
                git add -A .
                
                # recompute the number of changes and commit
                num=$(git status --porcelain | wc -l)
                git commit -m "auto-commit: new logs ($num files)"
            fi
        )
        ;;

esac
