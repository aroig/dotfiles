# Create $XDG_RUNTIME_DIR/shell
if [ "$XDG_RUNTIME_DIR" ]; then
    # create directory for the shell
    mkdir -m 700 "$XDG_RUNTIME_DIR/shell"

    # link the XDG_RUNTIME_DIR to $HOME
    ln -sfT "$XDG_RUNTIME_DIR" "$HOME/.runtime"
fi

