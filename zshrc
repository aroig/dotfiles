#------------------------------------------------------------------#
# File:     .zshrc   ZSH resource file                             #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


#------------------------------
# In case of tramp login
#------------------------------

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PS1='$ '
    return
fi


#------------------------------
# TTY Setup 
#------------------------------

set_tty_colors() {
    # Colors from .Xresources
    _SEDCMD='s/urxvt\.color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
    for i in $(sed -n "$_SEDCMD" $HOME/.Xresources | \
               awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}')
    do
        echo -en "$i"
    done
    echo -e "\\e]P0000000"
}


# Set tty colors on virtual console
if [[ "$TERM" = "linux" ]]; then
    set_tty_colors
fi 
    
# Disable ^S ^Q to stop start the output
stty stop undef
stty start undef


#------------------------------
# History stuff
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000


#------------------------------
# Modifying fpath
#------------------------------
fpath=($HOME/.zshrc.d/completions $fpath)


#------------------------------
# Set environment and aliases
#------------------------------

# Environment
if [ -f $HOME/.environment ]; then
    source $HOME/.environment
fi

# Aliases
if [ -f $HOME/.aliases ]; then
    source $HOME/.aliases
fi


#------------------------------
# Auto tmux
#------------------------------

# if ssh outside tmux
if [[ "$SSH_TTY" != "" && "$TMUX" == "" && "$NOTMUX" == "" ]]; then
    if which tmux 2>&1 >/dev/null; then
        tmux-session ssh
	exit 0
    else
	echo "tmux not installed. Starting zsh now"
    fi
fi


#------------------------------
# Loading generic stuff
#------------------------------

# Stop here if unknown terminal
case $TERM in
    rxvt*|screen*|xterm*|linux*)
	;;

    *)
	return
	;;
esac


# Load colors if possible
autoload -U colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi

autoload -U add-zsh-hook


#------------------------------
# Source files in .zshrc.d
#------------------------------

if [ -d $HOME/.zshrc.d ]; then
    ZSHRCD="$HOME/.zshrc.d"
    for src in $ZSHRCD/*.sh; do
	source $src
    done
fi
