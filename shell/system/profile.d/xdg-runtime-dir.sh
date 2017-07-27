# Create $XDG_RUNTIME_DIR/shell
if [ "$XDG_RUNTIME_DIR" ]; then
    # Create directory for the shell
    if [ ! -e "$XDG_RUNTIME_DIR/shell" ]; then
        mkdir -m 700 "$XDG_RUNTIME_DIR/shell"
    fi

    # Link the XDG_RUNTIME_DIR to $HOME
    if [ ! -e "$HOME/.runtime" ]; then
        ln -sfT "$XDG_RUNTIME_DIR" "$HOME/.runtime"
    fi
fi
