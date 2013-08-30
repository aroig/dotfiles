REMOTES=modes/markdown-mode modes/multi-mode modes/pkgbuild-mode \
        modes/rainbow-mode modes/yaml-mode packages/ibuffer-vc packages/powerline themes/zenburn

COMPILE=modes/markdown-mode modes/multi-mode modes/pkgbuild-mode \
        modes/rainbow-mode modes/yaml-mode packages/ibuffer-vc abdo

REMOTES_MERGE=$(patsubst %,%-merge,$(REMOTES))

EMACS_LISP=emacs-lisp

SHELL := $(SHELL) -e
MAKE=emacs -Q --batch -f batch-byte-compile

.PHONY: all doc $(REMOTES) $(REMOTES_MERGE) $(COMPILE)

all: $(COMPILE)

merge: $(REMOTES_MERGE)

doc: README.html


%.html: %.rst
	rst2html $? $@


$(REMOTES_MERGE): %-merge:
	git fetch $(shell basename -s -merge $@)
	git merge -s subtree --no-edit $(shell basename -s -merge $@)/master


$(COMPILE): %:
	cd $@; $(MAKE) *.el
	rm -f $(EMACS_LISP)/$(shell basename $@)
	ln -sfv ../$@ $(EMACS_LISP)/$(shell basename $@)

clean:
	rm -f *.html
	for r in $(COMPILE); do rm -f $$r/*.elc; done
