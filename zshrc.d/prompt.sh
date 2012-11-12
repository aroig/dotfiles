#------------------------------------------------------------------#
# File:     prompt.sh   Prompt                                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


promptwindow () {
    local window
    if [[ "$TMUX" != "" ]]; then
        window=$(tmux display-message -p '#I')
        window="%{$fg_bold[white]%}[$window]%{$reset_color%} "
    else
        window=""
    fi
    echo "$window"
}

promptuser () {
    local user_fmt=""
    case $(id -u -n) in
	root) user_fmt="%{$fg[red]%}$(id -u -n)"   ;;
           *) user_fmt="%{$fg[green]%}$(id -u -n)" ;;
    esac
    echo "$user_fmt%{$reset_color%}"
}


prompthost () {
    local HOST="$(hostname -s)"
    local host_fmt=""

    local orange
    if [[ "$TERM" == "linux" ]]; then orange="%{$fg[yellow]%}"
    else                              orange="$FX[bold]$FG[130]"
    fi

    case $HOST in
	grothendieck) host_fmt="%{$orange%}$HOST"           ;;
               hodge) host_fmt="%{$fg_bold[blue]%}$HOST"    ;;
              galois) host_fmt="%{$fg_bold[red]%}$HOST"     ;;
              skynet) host_fmt="%{$fg_bold[magenta]%}$HOST" ;;
                   *) host_fmt="%{$fg_bold[white]%}$HOST"   ;;
    esac
    echo "$host_fmt%{$reset_color%}"
}

promptsymbol () {
    local psymb='$'
    local promptcol

    if [[ "$USER" == "root" ]]; then psymb='#'
    fi

    if [[ "$1" == "0" ]]; then promptcol="$fg[white]"
    else                       promptcol="$fg_bold[red]"
    fi

    echo "%{$promptcol%}${psymb}%{$reset_color%}"
}


promptvcs () {
    local vctimeline
    case $__CURRENT_VCS_TIMELINE in
             sync) vctimeline="%{$fg_bold[green]%}=%{$reset_color%}" ;;
	    ahead) vctimeline="%{$fg_bold[blue]%}>%{$reset_color%}"  ;;
           behind) vctimeline="%{$fg_bold[red]%}<%{$reset_color%}"   ;;
 	divergent) vctimeline="%{$fg[red]%}Y%{$reset_color%}"        ;;
                *) vctimeline=""                                     ;;
    esac

    local vcstatus
    local cleancol stagedcol changedcol untrackedcol conflictcol unknowncol
    cleancol=$fg_bold[green]
    if [[ "$TERM" == "linux" ]]; then
           stagedcol="$fg[green]"
          changedcol="$fg_bold[red]"
        untrackedcol="$fg_bold[red]"
         conflictcol="$fg_bold[red]"
          unknowncol="$fg_bold[red]"

    else
           stagedcol="$fg[green]"
          changedcol="$FG[166]"
        untrackedcol="$FG[166]"
         conflictcol="$FG[124]"
          unknowncol="$FG[124]"
    fi

    case $__CURRENT_VCS_STATUS in
            clean) vcstatus="%{$cleancol%}√%{$reset_color%}"     ;;
 	   staged) vcstatus="%{$stagedcol%}*%{$reset_color%}"    ;;
          changed) vcstatus="%{$changedcol%}*%{$reset_color%}"   ;;
        untracked) vcstatus="%{$untrackedcol%}+%{$reset_color%}" ;;
	 conflict) vcstatus="%{$conflictcol%}X%{$reset_color%}"  ;;
                *) vcstatus="%{$unknowncol%}?%{$reset_color%}"   ;;
    esac

    local vcbranch="$__CURRENT_VCS_BRANCH"
    local vcrev="$__CURRENT_VCS_REV"

    local gitcol hgcol
    if [[ "$TERM" == "linux" ]]; then
        gitcol="$fg_bold[blue]"
         hgcol="$fg[red]"
    else
        gitcol="$fg[blue]"
         hgcol="$fg[red]"
    fi

    case $__CURRENT_VCS_PROGRAM in
        git) echo "${vcstatus}%{$gitcol%}(${vcbranch})${vctimeline}%{$reset_color%}" ;;
         hg) echo "${vcstatus}%{$hgcol%}(${vcrev})%{$reset_color%}"                  ;;
          *) echo ""                                                                 ;;
    esac
}

promptabdo () {

    local prompt
    case $__CURRENT_VCS_PROGRAM in
	git|hg) prompt="$(promptwindow)$(promptuser)@$(prompthost) $(promptvcs)"   ;;
	none|*) prompt="$(promptwindow)$(promptuser)@$(prompthost)"                ;;
    esac
    echo "$prompt"
}


promptdir () {
    local currdir="$(basename $PWD)"
    local user="$(id -n -u)"
    if [[ "$PWD" == "/home/$user" ]]; then currdir="~"; fi

    case $TERM in
	rxvt-unicode*) echo "%{$FX[italic]$fg_bold[yellow]%}$currdir%{$reset_color%}" ;;
                    *) echo "%{$fg_bold[yellow]%}$currdir%{$reset_color%}"            ;;
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

# Set the prompt
setopt prompt_subst
PROMPT='$(promptabdo) $(promptdir) $(promptsymbol $?) '

# PS2=$'%_>'
