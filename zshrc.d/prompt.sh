#------------------------------------------------------------------#
# File:     prompt.sh   Prompt                                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#



promptuser () {
    local user_fmt=""
    case $(id -u -n) in
	root)
	    user_fmt="%{$fg[red]%}$(id -u -n)"
	    ;;

	*)
	    user_fmt="%{$fg[green]%}$(id -u -n)"
	    ;;

    esac
    echo "$user_fmt%{$reset_color%}"
}


prompthost () {

    local HOST="$(hostname -s)"
    local host_fmt=""
    local col
    case $HOST in
	grothendieck)    
	    if [[ "$TERM" == "linux" ]]; then col="%{$fg[yellow]%}"
	    else col="%{$FX[bold]$FG[130]%}"
	    fi
	    host_fmt="$col$HOST"
	    ;;

        hodge)
	    host_fmt="%{$fg_bold[blue]%}$HOST"
	    ;;

	galois)
	    host_fmt="%{$fg_bold[red]%}$HOST"
	    ;;

	skynet)
	    host_fmt="%{$fg_bold[magenta]%}$HOST"
	    ;;

	*)
	    host_fmt="%{$fg_bold[white]%}$HOST"       
	    ;;

    esac
    echo "$host_fmt%{$reset_color%}"
}

promptsymbol () {

    local psymb='$'
    local prompt_fmt=""
    if [[ "$USER" == "root" ]]; then
	psymb='#'
    fi

#    if [[ $? == 0 ]]; then
#	prompt_fmt="%{$fg_bold[white]%}"
#    else
#        prompt_fmt="%{$fg_bold[red]%}"
#    fi
    echo "${prompt_fmt}${psymb}%{$reset_color%}"
}




promptvcs () {
    local vctimeline
    case $__CURRENT_VCS_TIMELINE in
	sync)
	    vctimeline="%{$fg_bold[green]%}=%{$reset_color%}"
	    ;;

	ahead)
#	    vctimeline="%{$fg[white]%}↑%{$reset_color%}"
	    vctimeline="%{$fg[white]%}>%{$reset_color%}"
	    ;;

	behind)
#	    vctimeline="%{$fg[white]%}↓%{$reset_color%}"
	    vctimeline="%{$fg[white]%}<%{$reset_color%}"
	    ;;

 	divergent)
	    vctimeline="%{$fg_bold[red]%}Y%{$reset_color%}"
	    ;;

	*)
	    vctimeline=""
	    ;;
    esac

    local vcstatus
    local col
    case $__CURRENT_VCS_STATUS in

	clean)
	    vcstatus="%{$fg_bold[green]%}√%{$reset_color%}"
	    ;;

	staged)
	    if [[ "$TERM" == "linux" ]]; then col="%{$fg[red]%}"
	    else col="%{$FG[136]%}"
	    fi
	    vcstatus="${col}●%{$reset_color%}"
	    ;;

	changed)
	    if [[ "$TERM" == "linux" ]]; then col="%{$fg_bold[red]%}"
	    else col="%{$FG[166]%}"
	    fi
	    vcstatus="${col}*%{$reset_color%}"
	    ;;

	untracked)
	    if [[ "$TERM" == "linux" ]]; then col="%{$fg_bold[red]%}"
	    else col="%{$FG[166]%}"
	    fi
	    vcstatus="${col}+%{$reset_color%}"
	    ;;

	conflict)
	    if [[ "$TERM" == "linux" ]]; then col="%{$fg_bold[red]%}"
	    else col="%{$FG[124]%}"
	    fi
	    vcstatus="${col}X%{$reset_color%}"
	    ;;

	*)
	    vcstatus="%{$fg_bold[red]%}?%{$reset_color%}"
	    ;;
    esac

    local vcbranch="$__CURRENT_VCS_BRANCH"

    local vcrev="$__CURRENT_VCS_REV"

    local vccolor
    case $__CURRENT_VCS_PROGRAM in
	git)
	    if [[ "$TERM" == "linux" ]]; then vccolor="%{$fg_bold[blue]%}"
	    else vccolor="%{$fg[blue]%}"
	    fi
	    echo "${vcstatus}${vccolor}(${vcbranch})${vctimeline}%{$reset_color%}"
	    ;;

	hg)
	    vccolor="%{$fg[red]%}"
	    echo "${vcstatus}${vccolor}(${vcrev})%{$reset_color%}"
	    ;;

	*)
	    echo ""
	    ;;
    esac
}

promptabdo () {
    local prompt
    local window
    if [[ "$TMUX" != "" ]]; then
	window=$(tmux display-message -p '#I')
    else
	window=""
    fi
    case $__CURRENT_VCS_PROGRAM in
	git|hg)
	    prompt="$(promptuser)@$(prompthost) $(promptvcs)"
	    ;;
	none|*)
	    prompt="$(promptuser)@$(prompthost)"
	    ;;
    esac

    case $TERM in
	screen*)
	    prompt="%{$fg_bold[white]%}[$window]%{$reset_color%} $prompt"
	    ;;
	
    esac

    echo "$prompt"
}


promptdir () {
    local currdir="$(basename $PWD)"
    local user="$(id -n -u)"
    if [[ "$PWD" == "/home/$user" ]]; then
	currdir="~"
    fi

    case $TERM in 
	rxvt-unicode*)
	    echo "%{$FX[italic]$fg_bold[yellow]%}$currdir%{$reset_color%}"
	    ;;

	*)
	    echo "%{$fg_bold[yellow]%}$currdir%{$reset_color%}"
	    ;;
    esac
}


messagehello() {
#    echo "$fg_bold[blue]Arch Linux.$reset_color"

    local creationdate
    if [[ "$TMUX" != "" ]]; then
	creationdate=$(tmux list-sessions | grep ssh | sed 's/^[0-9a-zA-Z :]*(created\s*\([^\[]*\)).*$/\1/g')

	if [[ "$creationdate" != "" ]]; then
	    echo "$fg_bold[yellow]Tmux$reset_color session created on $creationdate."
	    echo ""
	fi
    fi

}


# Print a welcome message
messagehello

setopt prompt_subst
PROMPT='$(promptabdo) $(promptdir) $(promptsymbol) '

# PS2=$'%_>'
