
EMACS   := emacsclient -s "$(XDG_RUNTIME_DIR)/emacs/server"


.PHONY: update sync




sync:
	$(EMACS) --eval '(configuration-layer/sync)'

update:
	$(EMACS) --eval '(configuration-layer/update-packages t)'

