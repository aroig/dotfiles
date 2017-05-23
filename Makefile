
SPACEMACS_DIR   := ~/.emacs.d
SPACEMACS_CACHE := ~/var/spacemacs

EMACS           := emacs --batch --load $(SPACEMACS_DIR)/init.el

# shell settings
SHELL       := /usr/bin/bash
.SHELLFLAGS := -e -u -c

.ONESHELL:

# So we can use $$(variable) on the prerequisites, that expand at matching time.
.SECONDEXPANSION:

FORCE:

.PHONY: update-init update sync pull clean-elpa

pull: $(SPACEMACS_DIR)/.git/FETCH_HEAD

update-init:
	@vimdiff $(SPACEMACS_DIR)/core/templates/.spacemacs.template init.el

sync:
	@$(EMACS) --eval '(configuration-layer/sync)'

update:
	@(
		cd $(SPACEMACS_CACHE)/elpa
		git checkout master
		$(EMACS) --eval '(configuration-layer/update-packages t)'
		$(EMACS) --eval '(configuration-layer/sync)'
		if [ -n "$$(git status --porcelain)" ]; then
			git add -A
			git commit -m 'Update packages'
		fi
	)

clean-elpa:
	@(
		cd $(SPACEMACS_CACHE)/elpa
		git ls-files -z | xargs -0 rm -f
	)
