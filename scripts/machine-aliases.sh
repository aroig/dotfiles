#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     machine-alias.sh   VM related aliases                  #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


spwn() {
    local machine="$1"
    local root="/media/$machine"

    if [ -f "$root/usr/bin/init" ]; then
        sudo systemd-nspawn --boot --network-bridge=brvirt --directory=$root

    else
        sudo systemd-nspawn --directory=$root
        
    fi
}

mchn() {
    local machine="$1"
    local config="$HOME/.config/systemd/conf/machines.d/$machine.conf"

    if [ ! -f "$config" ]; then
        echo "Can't find config file for machine $machine."
        return 1
    fi

    case "$machine" in
        tablet*|mobile*) systemctl --user start machine-android@$machine.service ;;
        *)               systemctl --user start machine-qemu@$machine.service ;;
    esac

    # TODO: ideally I should use socket units to fix this race!
    sleep 2 
}

mchnv() {
    local machine="$1"
    mchn "$machine"
    systemctl --user start machine-vnc@$machine.service
}

mchns() {
    local machine="$1"
    local config="$HOME/.config/systemd/conf/machines.d/$machine.conf"

    mchn "$machine"

    source "$config"
    telnet localhost "$PORT_SERIAL"  
}

mchnc() {
    local machine="$1"
    local config="$HOME/.config/systemd/conf/machines.d/$machine.conf"

    mchn "$machine"
    
    source "$config"
    socat - unix:$XDG_RUNTIME_DIR/qemu/$machine/control
}

mchnk() {
    local machine="$1"
    
    case "$machine" in
        tablet*|mobile*) systemctl --user stop machine-android@$machine.service ;;
        *)               systemctl --user stop machine-qemu@$machine.service ;;
    esac
}
