# arch package management
alias pac='sudo pacman'
alias pact='sudo pacman --config /etc/testing-pacman.conf'
alias pacs='sudo pacstrap -i'
alias pacr='pactree -c'
alias pacf='comm -13 <(pactree host-$(hostname)-cfg  -u | sort) <(pacman -Qsq | sort)'

alias achr='sudo arch-chroot'

pacsync() {
    local host="$1"
    rsync -avz --rsync-path='sudo rsync' "/var/cache/pacman/pkg/" "$host:/var/cache/pacman/pkg/"
    rsync -avz --rsync-path='sudo rsync' "/var/lib/pacman/sync/" "$host:/var/lib/pacman/sync/"
}
