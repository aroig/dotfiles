#!/bin/bash

if [ ! "$1" ]; then 
    echo "No session specified."
    exit 1
fi

session="$1"
shift

if [ -f ~/.tmux/$session ]; then  opts_file="$HOME/.tmux/$session"
else                              opts_file="$HOME/.tmux/default"
fi

# create a new master session detached from the world
tmux_master(){
    tmux new-session -d -s $session\; source-file "$opts_file"\; "$@"
}

# create a new client session attached to an existing one
tmux_client(){
    tmux new-session -t $session -s $session-$$\; \
        source-file "$opts_file"\; set destroy-unattached on\; "$@"
}

( tmux_client && exit 0 ) || ( tmux_master && tmux_client && exit 0)
