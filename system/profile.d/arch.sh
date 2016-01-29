# arch package management
alias pac='sudo pacman'
alias pas='sudo pacstrap -i'
alias pat='pactree -c'
alias paf='comm -13 <(pactree host-$(hostname)-cfg  -u | sort) <(pacman -Qsq | sort)'
alias cow='cower'
alias ach='sudo arch-chroot'


pacsync() {
    local host="$1"
    rsync -avz --rsync-path='sudo rsync' "/var/cache/pacman/pkg/" "$host:/var/cache/pacman/pkg/"
    rsync -avz --rsync-path='sudo rsync' "/var/lib/pacman/sync/" "$host:/var/lib/pacman/sync/"
}
