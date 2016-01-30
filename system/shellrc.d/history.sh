
HISTFILE="$XDG_RUNTIME_DIR/shell/bash_history"
HISTSIZE=1000
SAVEHIST=1000

if [ "$BASH_VERSION" ]; then
    # extended globbing
    shopt -s extglob


    # Avoid duplicates
    export HISTCONTROL=ignoredups:erasedups  

    shopt -s histappend

    sync_history() { history -a; history -c; history -r; }
fi

if [ "$ZSH_VERSION" ]; then
    setopt incappendhistory 
    setopt sharehistory
    setopt extendedhistory
fi
