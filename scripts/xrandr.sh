#!/bin/bash

# set variables
action="$1"
arg="$2"
host=$(hostname -s)

# all outputs
OUTS=$(xrandr -q | grep "connected" | sed 's/^\([A-Za-z0-9\-]*\).*/\1/')

# Connected outputs
COUTS=$(xrandr -q | grep " connected" | sed 's/^\([A-Za-z0-9\-]*\).*/\1/')

# match the right output
match_out() {
    echo $OUTS | sed 's/\s/\n/g' | grep "$1"
}


case $host in
    grothendieck)
	    primary=$(match_out VGA)
	    secondary=$(match_out DVI)
	    tertiary=$(match_out HDMI)
	    ;;
    
    hodge)
	    primary=$(match_out DVI)
	    secondary=""
	    tertiary=""
	    ;;
    
    galois)
	    primary=$(match_out eDP1)
        secondary=$(match_out HDMI1)
        tertiary=$(match_out HDMI2)
	    ;;
esac

# default action
if [ ! "$action" ]; then
    action=single
fi

if [ "$action" == "auto" ]; then
    case "$host" in
        grothendieck) action=dual     ;;
        hodge)        action=single   ;;
        galois)       action=single   ;;
    esac
fi

# screen options
case $action in 
    dual)
	    echo "xinerama: $primary, $secondary"
	    xrandr --output $primary --auto --primary --output $secondary --auto --right-of $primary --output $tertiary --off
	    ;;
	
    mirror)
	    echo "mirror: $primary, $secondary"
	    xrandr --output $primary --auto --primary --output $secondary --auto --same-as $primary --output $tertiary --off
	    ;;

	hdmi)
        echo "mirror: $primary, $tertiary"
	    xrandr --output $primary --auto --primary --output $tertiary --auto --right-of $primary --output $secondary --off
        ;;

    single)
	    echo "single: $primary"
	    xrandr --output $primary --auto --primary --output $secondary --off --output $tertiary --off
	    ;;
esac
