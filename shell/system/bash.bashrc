#
# /etc/bash.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

PS1='[\u@\h \W]\$ '
PS2='> '
PS3='> '
PS4='+ '

# Source profiles from /etc/shellrc.d
if test -d /etc/shellrc.d/; then
    for scr in /etc/shellrc.d/*.sh; do
        test -r "$scr" && source "$scr"
    done
    unset scr
fi

[ -r /usr/share/bash-completion/bash_completion ] && source /usr/share/bash-completion/bash_completion
