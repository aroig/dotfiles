#------------------------------------------------------------------#
# File:     prompt.sh   Prompt adjustment                          #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

#Set rxvt title
case "$TERM" in
    xterm*|rxvt*)
        XTERM_TITLE='\[\e]0;\u@\H: \W\a\]'
        ;;
esac

PROMPT_COMMAND='RET=$?;'
RET_VALUE='$(echo $RET)'
RET_COLOR='$(if [[ $RET == 0 ]]; then echo -ne "\[$BWhite\]"; else echo -ne "\[$BRed\]"; fi)'


case "$(hostname -s)" in
    galois)
	HOST_COLOR="\[$BRed\]"
	;;

    hodge)
	HOST_COLOR="\[$BBlue\]"
	;;

    grothendieck)
	HOST_COLOR="\[$Yellow\]"
	;;

    skynet)
	HOST_COLOR="\[$BPurple\]"
	;;
esac


case "$(id -u -n)" in
    abdo)
	USER_COLOR="\[$Green\]"
	;;

    root)
	USER_COLOR="\[$Red\]"
	;;

    *)
	USER_COLOR="\[$White\]"
	;;
esac

AT_COLOR="\[$White\]"
DIR_COLOR="\[$Color_Off\]"


# The prompt
PS1="$XTERM_TITLE$USER_COLOR\u$AT_COLOR@$HOST_COLOR\h $DIR_COLOR\W $Blue\$\[$Color_Off\] "

