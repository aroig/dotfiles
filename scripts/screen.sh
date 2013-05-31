#!/bin/bash
# set -o errexit

# set variables
action="$1"
arg="$2"
host=$(hostname -s)

# all outputs
OUTS=$(xrandr -q | grep "connected" | sed 's/^\([A-Za-z0-9\-]*\).*/\1/')

# Connected outputs
COUTS=$(xrandr -q | grep " connected" | sed 's/^\([A-Za-z0-9\-]*\).*/\1/')

LVDS=$(echo $OUTS  | sed 's/\s/\n/g' | grep LVDS)
DVI=$(echo $OUTS  | sed 's/\s/\n/g' | grep DVI)
VGA=$(echo $OUTS  | sed 's/\s/\n/g' | grep VGA)
HDMI=$(echo $OUTS  | sed 's/\s/\n/g' | grep HDMI)


case $host in
    grothendieck)
	    primary=$VGA
	    secondary=$DVI
	    tertiary=$HDMI
        dpms_timeout="0 0 0"
	    ;;
    
    hodge)
	    primary=$DVI
	    secondary=""
	    tertiary=""
        dpms_timeout="0 0 0"        
	    ;;
    
    galois)
	    primary=$LVDS
	    if [ "$arg" == "hdmi" ]; then
	        secondary=$HDMI
	        tertiary=$VGA
	    else
	        secondary=$VGA
	        tertiary=$HDMI
	    fi
        dpms_timeout="300 600 900"
	    ;;
esac

# default action
if [ ! "$action" ]; then
    action=single
fi

if [ "$action" == "default" ]; then
    case "$host" in
        grothendieck) action=xinerama ;;
        hodge)        action=single   ;;
        galois)       action=single   ;;
    esac
fi

# screen options
case $action in 
    xinerama)
	    echo "xinerama: $primary, $secondary"
	    xrandr --output $primary --auto --primary --output $secondary --auto --right-of $primary --output $tertiary --off
        xset dpms $dpms_timeout s 0 600      # set dpms and screensaver        
	    ;;
	
    mirror)
	    echo "mirror: $primary, $secondary"
	    xrandr --output $primary --auto --primary --output $secondary --auto --same-as $primary --output $tertiary --off
        xset dpms $dpms_timeout s 0 600      # set dpms and screensaver        
	    ;;
	
    single)
	    echo "single: $primary"
	    xrandr --output $primary --auto --primary --output $secondary --off --output $tertiary --off
        xset dpms $dpms_timeout s 0 600      # set dpms and screensaver        
	    ;;
esac
