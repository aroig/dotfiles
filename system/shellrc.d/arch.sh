# arch package management
alias pac='sudo systemd-inhibit --who=pacman --why="system upgrade" pacman'
alias pact='sudo systemd-inhibit --who=pacman --why="system upgrade" pacman --config /etc/testing-pacman.conf'
alias pacs='sudo pacstrap -i'
alias pacr='pactree -c'
alias pacf='comm -13 <(pactree host-$(hostname)-cfg  -u | sort) <(pacman -Qsq | sort)'

alias achr='sudo arch-chroot'

pacsync() {
    local host="$1"
    rsync -avz --rsync-path='sudo rsync' "/var/cache/pacman/pkg/" "$host:/var/cache/pacman/pkg/"
    rsync -avz --rsync-path='sudo rsync' "/var/lib/pacman/sync/" "$host:/var/lib/pacman/sync/"
}
