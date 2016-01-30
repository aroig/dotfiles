
# Stop here if unknown terminal
case $TERM in
    rxvt*|xterm*|eterm-color)   # graphical terminal
        ;; 
    
    screen*|linux*)             # tty or screen / tmux.
        unset DISPLAY
        ;;  
    
    vt*)                        # serial console.
        unset DISPLAY
        ;;  
    
    *)                          # unknown terminal, stop right here.
        ;;   
esac
