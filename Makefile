REMOTES=modes/git-modes modes/markdown-mode modes/multi-mode modes/pkgbuild-mode modes/rainbow-mode modes/yaml-mode packages/ibuffer-vc packages/powerline themes/zenburn
REMOTES_MERGE=$(patsubst %,%-merge,$(REMOTES))
EMACS_LISP=emacs-lisp


SHELL := $(SHELL) -e
MAKE=emacs -Q --batch -f batch-byte-compile

.PHONY: all links doc $(REMOTES) $(REMOTES_MERGE)


%.html: %.rst
	rst2html $? $@


$(REMOTES_MERGE): %-merge:
	git fetch $(shell basename -s -merge $@)
	git merge -s subtree --no-edit $(shell basename -s -merge $@)/master


$(REMOTES): %:
	cd $@; $(MAKE) *.el
	rm -f $(EMACS_LISP)/$(shell basename $@)
	ln -sfv $@ $(EMACS_LISP)/$(shell basename $@)


all: $(REMOTES)

merge: $(patsubst %,%-merge,$(REMOTES))

doc: README.html

clean:
	rm -f *.html
