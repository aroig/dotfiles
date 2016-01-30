#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     tmux.sh      Helper tmux functions                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#



# create a new client session attached to an existing one, and then create a new window
tmux_client() {
    session="$1"
    [ ! "$session" ] && session="default"
    shift

    tmux -S "$XDG_RUNTIME_DIR/tmux/$session" \
        new-session -t $session -s $session-$$\; \
        new-window\; \
        set destroy-unattached on\; "$@"
}


# create a new tmux session attached to an existing one master session
# launching the master if necessary
tmux_session() {
    session="default"

    ( tmux_client "$session" && \
      exit 0 
    ) || \
    ( systemctl --user start tmux.service && \
      tmux_client "$session" && \
      exit 0 
    )
}

