#!/usr/bin/bash

# source zshenv when not in zsh.
if [ ! "$ZSH_VERSION" ]; then
    if [ -f "/etc/zshenv" ]; then
        source "/etc/zshenv"
    fi

    if [ -f "$HOME/.zshenv" ]; then
        source "$HOME/.zshenv"
    fi
fi
