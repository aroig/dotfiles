#!/bin/bash

if [ ! "$1" ]; then           session=default
else                          session="$1"
fi

if [ -f ~/tmux.d/$session ]; then  opts_file="$HOME/.tmux.d/$session"
else                               opts_file="$HOME/.tmux.d/default"
fi

tmux attach-session -t "$session" ||
tmux new-session    -s "$session" \; source-file "$opts_file"
