#!/bin/zsh
#------------------------------------------------------------------#
# File:     gnupg.zsh      Helper gnupg functions                  #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

# sets the default keyring to use
#  default: the one at ~/.gnupg
#  master:  the one at ~/priv/gnupg
#
# TODO: what about the gpg-agent sockets?
gpg_keyring() {
    case "$1" in
       master)
            export GPGHOME="$AB2_PRIV_DIR/gnupg"
            ;;
        
        default)
            export GPGHOME="$HOME/.gnupg"
            ;;

        *)
            echo "$GPGHOME"
            ;;
    esac
}


# forwards requests to the agent from a remote
gpg_agent_forward() {
    remote="$1"
    gpg_socket="${GPG_AGENT_INFO%::1}"
    gpg_forward_socket=/home/abdo/.gnupg/S.gpg-agent.forward
    socket_cmd="socat UNIX-LISTEN:${gpg_forward_socket},unlink-close,unlink-early STDIO"
    socat  SYSTEM:"ssh -S none $remote \"$socket_cmd\"" UNIX-CONNECT:"$gpg_socket"
}

ssh_agent_forward() {
    remote="$1"
    ssh_socket="$SSH_AUTH_SOCK"
    ssh_forward_socket=/home/abdo/.gnupg/S.gpg-agent.ssh.forward
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

