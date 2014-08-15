#!/bin/zsh

ZSHENV=~/.zshenv
source $ZSHENV

variables=( `cat ~/.zshenv | egrep -o '^\s*export\s*.*=' | sed 's/^\s*export\s*\([^=]*\)=.*$/\1/'` )

if [ "$variables" ]; then
    systemctl --user import-environment $variables
fi
