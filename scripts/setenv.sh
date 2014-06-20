#!/bin/zsh

ZSHENV=~/.zshenv
source $ZSHENV

cat ~/.zshenv | egrep -o '^\s*export\s*.*=' | sed 's/^\s*export\s*\([^=]*\)=.*$/\1/' | \
while read key; do
    eval value=\"\$$key\"
    # echo "$key=$value"    
    systemctl --user set-environment "$key=$value"
done



