#!/bin/zsh

ZSHENV=~/.zshenv
source $ZSHENV

for key in `cat ~/.zshenv | egrep -o '^\s*export\s*.*=' | sed 's/\s*export\s*\(.*\)=/\1/'`; do
    eval value=\$$key
#    echo $key=$value
    systemctl --user set-environment $key="$value"
done



