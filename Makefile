
EMACS   := emacs --batch --load ~/devel/elisp/spacemacs/init.el

SPACEMACS_DIR   := ~/emacs.d
SPACEMACS_CACHE := ~/.emacs.d/.cache

# shell settings
SHELL       := /usr/bin/bash
.SHELLFLAGS := -e -u -c

.ONESHELL:

# So we can use $$(variable) on the prerequisites, that expand at matching time.
.SECONDEXPANSION:


.PHONY: update-init update sync


update-init:
	vimdiff $(SPACEMACS_DIR)/core/templates/.spacemacs.template init.el

sync:
	$(EMACS) --eval '(configuration-layer/sync)'

update:
	$(EMACS) --eval '(configuration-layer/update-packages t)'
	$(EMACS) --eval '(configuration-layer/sync)'
	(
		cd ${SPACEMACS_CACHE}/elpa
		git add -A
		git commit -m 'Update packages'
	)

