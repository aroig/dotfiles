
EMACS   := emacsclient -s "$(XDG_RUNTIME_DIR)/emacs/server"


.PHONY: update-init update sync


update-init:
	vimdiff ~/devel/elisp/spacemacs/core/templates/.spacemacs.template init.el

sync:
	$(EMACS) --eval '(configuration-layer/sync)'

update:
	$(EMACS) --eval '(configuration-layer/update-packages t)'

