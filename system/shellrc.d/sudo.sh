
#------------------------------
# sudo
#------------------------------

# use policykit to do sudo
alias pke="pkexec"

# If the argument has an alias, expand it, if it has a function, 
# run it on a root shell and if no arguments given, go to a root shell.
sudo () {   
    if [[ -n "$1" ]]; then
        if alias "$1" 2>/dev/null >/dev/null; then
            /usr/bin/sudo $SHELL -ic "$*"

        elif type "$1" | grep -q 'function' 2>/dev/null >/dev/null; then
            /usr/bin/sudo $SHELL -ic "$*"

        else
            /usr/bin/sudo "$@"
        fi
    else
        /usr/bin/sudo $SHELL
    fi
}

# alias sudo='sudo '        # Enables expanding aliases for next command. not functions though :(


