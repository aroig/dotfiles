#!/usr/bin/bash

# source zshenv when not in zsh.
if [ ! "$ZSH_VERSION" ]; then
    if [ -f "/etc/zsh/zshenv" ]; then
        source "/etc/zsh/zshenv"
    fi

    if [ -f "$HOME/.zshenv" ]; then
        source "$HOME/.zshenv"
    fi
fi
