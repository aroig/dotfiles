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
# Auto tmux
#------------------------------

# if ssh outside tmux
if [[ "${SSH_TTY}" != "" && "${TMUX}" == "" && "$NOTMUX" == "" ]]; then     
    if which tmux 2>&1 >/dev/null; then

	if [[ "$(tmux has-session -t ssh 2> /dev/null; echo $?)" == "0" ]]; then
	    tmux attach -t ssh
	else
	    tmux new -s ssh
	fi

	exit 0
    else
	echo "tmux not installed. Starting zsh now"
    fi
fi


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


# Stop here if unknown terminal
case $TERM in
    rxvt*|screen*|xterm*|linux*)
	;;

    *)
	return
	;;
esac


#------------------------------
# Loading generic stuff
#------------------------------

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
    for src in $HOME/.zshrc.d/*.sh; do
	source $src
    done
fi
