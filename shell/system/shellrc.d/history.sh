
HISTSIZE=1000
SAVEHIST=1000

# bash history file
if [ "$BASH_VERSION" ] && [ -n "$XDG_RUNTIME_DIR" ]; then
    HISTFILE="$XDG_RUNTIME_DIR/shell/bash_history"
    # extended globbing
    shopt -s extglob


    # Avoid duplicates
    export HISTCONTROL=ignoredups:erasedups

    shopt -s histappend

    sync_history() { history -a; history -c; history -r; }
fi

# zsh history file
if [ "$ZSH_VERSION" ] && [ -n "$XDG_RUNTIME_DIR" ]; then
    HISTFILE="$XDG_RUNTIME_DIR/shell/zsh_history"
    setopt incappendhistory
    setopt sharehistory
    setopt extendedhistory
fi

# Disable history if no XDG_RUNTIME_DIR is set
if [ -z "$XDG_RUNTIME_DIR" ]; then
    unset HISTFILE
fi
