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
    xdg-open "$@" &> /dev/null &!
}

rf()  {
    rifle "$@"
}

# terminal editor
vi()  {
    $EDITOR "$@";
}

# emacs
ee()  {
    if [ "$1" ]; then sdrun $EMACS "$@"
    else              sdrun $EMACS "$PWD"
    fi
}

# emacs project
ep() {
    if [ "$1" ]; then sdrun $EMACS -e "(ab2/find-file-in-project \"$1\")"
    else              sdrun $EMACS -e "(ab2/find-file-in-project \"$1\")"
    fi
}

# open magit
mg() {
    if [ "$1" ]; then sdrun $EMACS -e "(abdo-vcs-main \"$1\")"
    else              sdrun $EMACS -e "(abdo-vcs-main \"$PWD\")"
    fi
}

# open tmux session
tx() {
    if [ "$TMUX" ]; then   tmux new-window
    else                   tmux_session default
    fi
}

# detach from tmux
dt() {
    if [ "$TMUX" ]; then   tmux detach-client
    fi
}

# if inside tmux close window and detach, otherwise just exit
cl() {
    if [ "$TMUX" ]; then   tmux unlink-window -k\; detach-client
    else                   exit 0
    fi
}

# new terminal
tm()  {
    if [ "$1" ]; then sdrun $TERMCMD -d "$1"
    else              sdrun $TERMCMD -d "$PWD"
    fi
}

# ranger session
rg()  {
    if [ "$1" ]; then sdrun $TERMCMD -e ranger -d "$1"
    else              sdrun $TERMCMD -e ranger -d "$PWD"
    fi
}

# open file manager
fm()  {
    if [ "$1" ]; then sdrun $TERMCMD -e vifm -d "$1"
    else              sdrun $TERMCMD -e vifm -d "$1"
    fi
}

# TODO: open vifm dropdown

# These commands open awesome dropdown clients
rgd() {
    echo "ddclient.ranger:newtab('$PWD')"   | awesome-client
}

tmd() {
    echo "ddclient.terminal:newtab('$PWD')" | awesome-client
}


