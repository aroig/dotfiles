#------------------------------
# Program shortcuts
#------------------------------

# NOTE1: &! detaches and disowns process. The shell no longer keeps track of it.

# NOTE2: rifle always expects arguments

# NOTE3: rifle either completely detaches or runs a terminal program in-place.

# NOTE4: The following commands are aware of the type of terminal they are on. For
# example fm launches GTK file manager on any terminal except tty's or remote logins


# file openers
op() {
    realpath -z "$@" | xargs -r -0 xdg-open
}

# emacs
ee()  {
    eval "local CMD=($EMACS)"
    if [ "$1" ]; then sdrun "${CMD[@]}" "$@"
    else              sdrun "${CMD[@]}" "$PWD"
    fi
}

# emacs project
ep() {
    eval "local CMD=($EMACS)"
    if [ "$1" ]; then sdrun "${CMD[@]}" -e "(ab2/find-file-in-project \"$1\")"
    else              sdrun "${CMD[@]}" -e "(ab2/find-file-in-project \"$PWD\")"
    fi
}

# emacs on the tty
et()  {
    eval "local CMD=(${EMACS/-c/-tty})"
    if [ "$1" ]; then "${CMD[@]}" "$@"
    else              "${CMD[@]}" "$PWD"
    fi
}

# open magit
eg() {
    eval "local CMD=($EMACS)"
    if [ "$1" ]; then sdrun "${CMD[@]}" -e "(abdo-vcs-main \"$1\")"
    else              sdrun "${CMD[@]}" -e "(abdo-vcs-main \"$PWD\")"
    fi
}

# open new tmux session
xx() {
    local session="default"
    if [ "$TMUX" ]; then
        tmux new-window -c "${PWD}"
    else
        systemctl --user start tmux.service
        tmux -S "$XDG_RUNTIME_DIR/tmux/default" \
             new-session -A -t "$session" -c "${PWD}" -s "$session-$$" \; \
             set destroy-unattached on \; "$@"
    fi
}

# detach from tmux
xd() {
    if [ "$TMUX" ]; then   tmux detach-client
    fi
}

# if inside tmux close window and detach, otherwise just exit
xc() {
    if [ "$TMUX" ]; then   tmux unlink-window -k \; detach-client
    else                   exit 0
    fi
}

# new terminal
tm()  {
    eval "local CMD=($TERMCMD)"
    if [ "$1" ]; then sdrun "${CMD[@]}" -e "$SHELL -l" -d "$1"
    else              sdrun "${CMD[@]}" -e "$SHELL -l" -d "$PWD"
    fi
}

# vifm as an awesome dropdown
fm() {
    sdrun vifm-dropdown.service
    # TODO: remove this once vifm is socket-activated
    sleep 0.5

    if [ "$1" ]; then local dir=$(realpath "$1")
    else              local dir=$(realpath "$PWD")
    fi

    vifm --remote -c "cd '$dir'"
}

# OTP keys
totp() {
    local priv_otp="${HOME}/priv/etc/otp"
    if [ -f "${priv_otp}/$1" ]; then
        oathtool -b "$(cat ${priv_otp}/$1)"
    else
        echo "Available services:"
        ls "${priv_otp}/"
    fi
}
