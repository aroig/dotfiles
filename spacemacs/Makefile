
SPACEMACS_DIR   := ~/.emacs.d
SPACEMACS_CACHE := ~/.emacs.d/.cache

EMACS           := emacs --batch --load $(SPACEMACS_DIR)/init.el

# shell settings
SHELL       := /usr/bin/bash
.SHELLFLAGS := -e -u -c

.ONESHELL:

# So we can use $$(variable) on the prerequisites, that expand at matching time.
.SECONDEXPANSION:

FORCE:

.PHONY: update-init update sync pull

$(SPACEMACS_DIR)/.git/FETCH_HEAD: FORCE
	@(
		cd $(SPACEMACS_DIR)
		git pull
	)


pull: $(SPACEMACS_DIR)/.git/FETCH_HEAD

update-init:
	@vimdiff $(SPACEMACS_DIR)/core/templates/.spacemacs.template init.el

sync:
	@$(EMACS) --eval '(configuration-layer/sync)'

update: $(SPACEMACS_DIR)/.git/FETCH_HEAD
	@(
		cd $(SPACEMACS_CACHE)/elpa
		git checkout master
		$(EMACS) --eval '(configuration-layer/update-packages t)'
		$(EMACS) --eval '(configuration-layer/sync)'
		git add -A
		git commit -m 'Update packages'
	)

