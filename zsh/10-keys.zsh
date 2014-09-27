#!/bin/zsh

#------------------------------
# Bind some special keys
#------------------------------

# NOTE: on a terminal do C-v + key, to see the escaped character!

# NOTE: This is specific to termite, and does not seem to work... oh well

# emacs like bindings
bindkey -e

case $TERM in
    xterm-termite*)    
        bindkey "\e[H"    beginning-of-line            # [Home] - move to beginning of line
        bindkey "\e[F"    end-of-line                  # [End]  - move to the end of line
        
        bindkey "\e[1;5C" forward-word                 # [C-Right] - move forward one word
        bindkey "\e[1;5D" backward-word                # [C-Left]  - move backward one word
        ;;
esac
