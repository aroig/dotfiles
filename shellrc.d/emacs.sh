
# emacs configuration
em() {
    rm -f ~/.emacs.d
    ln -sf etc/emacs ~/.emacs.d
    emacs "$@"
}

# spacemacs config
se() {
    rm -f ~/.emacs.d
    ln -sf devel/elisp/spacemacs ~/.emacs.d
    emacs "$@"
}
