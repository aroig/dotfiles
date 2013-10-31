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
            METADATA=$(find metadata*)

            # if metadata has changed
            if [[ $(git status --porcelain $METADATA | wc -l) -ge 1 ]]; then
                git reset -q                        # unstage everything
                git annex edit metadata.db          # make sure we are editing metadata.db
                oldkey=$(basename `git show HEAD:metadata.db`)

                # annex and commit
                git annex add $METADATA     
                git add $METADATA
                git commit -m "Update metadata.db" 

                # drop old metadata.db
                newkey=$(basename `git show HEAD:metadata.db`)
                if [ ! "$oldkey" == "$newkey" ]; then
                    git annex dropkey --force "$oldkey"         
                fi
            fi

            # if changed files remain, commit
            num=$(git status --porcelain | wc -l)
            if [[ $num -ge 1 ]]; then
                git annex add .
                git add -A .
                git commit -m "Update library ($num files)"
            fi
        )
        ;;

esac
