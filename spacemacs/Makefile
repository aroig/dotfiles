
EMACS   := emacs --batch --load ~/devel/elisp/spacemacs/init.el 


.PHONY: update-init update sync


update-init:
	vimdiff ~/devel/elisp/spacemacs/core/templates/.spacemacs.template init.el

sync:
	$(EMACS) --eval '(configuration-layer/sync)'

update:
	$(EMACS) --eval '(configuration-layer/update-packages t)'
	$(EMACS) --eval '(configuration-layer/sync)'

