#!/usr/bin/zsh

if [ "$ZSH_VERSION" ]; then
    # super globs
    setopt extendedglob
    unsetopt caseglob

    # pound sign in interactive prompt
    setopt interactivecomments 

    # Display CPU usage after $REPORTTIME seconds
    REPORTTIME=10
fi
