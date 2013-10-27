REMOTES=modes/markdown-mode modes/multi-mode modes/pkgbuild-mode \
        modes/rainbow-mode modes/yaml-mode packages/ibuffer-vc packages/powerline themes/zenburn

COMPILE=modes/markdown-mode modes/multi-mode modes/pkgbuild-mode \
        modes/rainbow-mode modes/yaml-mode packages/ibuffer-vc abdo

REMOTES_MERGE=$(patsubst %,%-merge,$(REMOTES))

EMACS_LISP=emacs-lisp

SHELL := $(SHELL) -e
MAKE=emacs -Q --batch -f batch-byte-compile

.PHONY: all doc $(REMOTES) $(REMOTES_MERGE) $(COMPILE)

all: $(COMPILE) doc

merge: $(REMOTES_MERGE)

doc: README.html

%.html: %.rst
	rst2html $? $@

$(REMOTES_MERGE): %-merge:
	git fetch $(shell basename -s -merge $@)
	@remote=$(shell basename -s -merge "$@")/master;      \
	path=$(patsubst %-merge,%,$@);                        \
	echo "subtree merging into $$path";                   \
	if [ ! -d "$$path" ]; then                            \
	  git merge -s ours --no-commit "$$remote";           \
	  git read-tree --prefix="$path" -u "$$remote";       \
	  git commit -m "Initialize subtree at '$$path'";     \
	fi;                                                   \
	git merge -s recursive -X subtree="$$path"            \
	  -m "Merge subtree '$$remote'" "$$remote"

$(COMPILE): %:
	cd $@; $(MAKE) *.el
	rm -f $(EMACS_LISP)/$(shell basename $@)
	ln -sfv ../$@ $(EMACS_LISP)/$(shell basename $@)

clean:
	rm -f *.html
	for r in $(COMPILE); do rm -f $$r/*.elc; done
