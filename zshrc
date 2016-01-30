#!/usr/bin/zsh

# source zshenv
for src in "/etc/zsh/zshenv" "$HOME/.zshenv"; do
    test -r "$src" && source "$src"
done
unset src

# source profiles from /etc/zsh/zshrc.d if not already sourced
if test -d "$AB2_CONF_DIR/shell/shellrc.d"; then
    for scr in "$AB2_CONF_DIR/shell/shellrc.d"/*.sh; do
        test -r "$scr" && source "$scr"
    done
    unset scr
fi
