#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     gnupg.sh      Helper gnupg functions                   #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


# forwards requests to the agent from a remote
gpg_agent_forward() {
    remote="$1"
    gpg_socket="${GPG_AGENT_INFO%::1}"
    gpg_forward_socket="${HOME}/.gnupg/S.gpg-agent.forward"
    socket_cmd="socat UNIX-LISTEN:${gpg_forward_socket},unlink-close,unlink-early STDIO"
    socat  SYSTEM:"ssh -S none $remote \"$socket_cmd\"" UNIX-CONNECT:"$gpg_socket"
}

ssh_agent_forward() {
    remote="$1"
    ssh_socket="$SSH_AUTH_SOCK"
    ssh_forward_socket="${HOME}/.gnupg/S.gpg-agent.ssh.forward"
    socket_cmd="socat UNIX-LISTEN:${ssh_forward_socket},unlink-close,unlink-early STDIO"
    socat  SYSTEM:"ssh -S none $remote \"$socket_cmd\"" UNIX-CONNECT:"$ssh_socket"
}


# sets the mode for the gpg-agent
#  tty:     we are on a local tty
#  graphic: we are on a grahic environment

gpg_agent_mode() {

    case "$1" in
        tty)
            ssh_sock="$HOME/.gnupg/S.gpg-agent.ssh"
            gpg_sock="$HOME/.gnupg/S.gpg-agent"       
            ;;

        graphic)
            ssh_sock="$HOME/.gnupg/S.gpg-agent.ssh"
            gpg_sock="$HOME/.gnupg/S.gpg-agent"           
            ;;

          ssh)
            ssh_sock="$HOME/.gnupg/S.gpg-agent.ssh"
            gpg_sock="$HOME/.gnupg/S.gpg-agent"
            ;;

      forward)
            ssh_sock="$HOME/.gnupg/S.gpg-agent.ssh.forward"
            gpg_sock="$HOME/.gnupg/S.gpg-agent.forward"
            ;;

        *)
            echo "SSH_AUTH_SOCK=$SSH_AUTH_SOCK"
            echo "GPG_AGENT_INFO=$GPG_AGENT_INFO"
            ;;
    esac

    export SSH_AUTH_SOCK="$ssh_sock"
    export GPG_AGENT_INFO="$gpg_sock::1"

    case "$1" in
        tty|graphic)
            gpg_agent_update
            ;;
    esac
}


# Update tty for the gpg-agent. gpg itself can be aware of the tty,
# maybe via GPG_TTY environment var, but there is no way that programs using
# gpg-agent as a ssh-agent can tell it about the tty. So we are stuck with this.
gpg_agent_update() {
    export GPG_TTY=$(tty)
    gpg-connect-agent updatestartuptty /bye
}

# 
gpg_agent_passphrase() {
    
    case "$1" in
        -s|--set)
            read -s "?Passphrase: " passwd            
            ;;

        *)
            echo "Usage: gpg_agent_passphrase [ --set | --forget ]"
            return 0
            ;;
    esac

    gpg --with-fingerprint --with-fingerprint --with-colons --fixed-list-mode --list-keys | \
    grep fpr | sed 's/fpr:*\([^:]*\):/\1/' | \
    while read fingerprint; do
        case "$1" in
            -f|--forget)
                /usr/lib/gnupg/gpg-preset-passphrase --forget "$fingerprint"
                ;;

            -s|--set)
                /usr/lib/gnupg/gpg-preset-passphrase --preset "$fingerprint" <<< "$passwd"
                ;;
        esac
    done
}
