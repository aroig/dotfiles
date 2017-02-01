
HISTSIZE=1000
SAVEHIST=1000

if [ "$BASH_VERSION" ]; then
    HISTFILE="$XDG_RUNTIME_DIR/shell/bash_history"
    # extended globbing
    shopt -s extglob


    # Avoid duplicates
    export HISTCONTROL=ignoredups:erasedups

    shopt -s histappend

    sync_history() { history -a; history -c; history -r; }
fi

if [ "$ZSH_VERSION" ]; then
    HISTFILE="$XDG_RUNTIME_DIR/shell/zsh_history"
    setopt incappendhistory
    setopt sharehistory
    setopt extendedhistory
fi
