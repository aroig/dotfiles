# ----------------------------------------------------------------------- #
# Abdó Roig-Maranges <abdo.roig@gmail.com>                                #
# Makefile to compile my dotfiles directory for github. Merges changes    #
# from several locar repos on my machine into a single dotfiles repo      #
# using the git merge -s subtree.                                         #
# ----------------------------------------------------------------------- #

REMOTES=awesome emacs ranger shell systemd vim zathura

SHELL := $(SHELL) -e

.PHONY: all merge doc $(REMOTES)

all: merge doc

merge: $(REMOTES)

doc: README.html

$(REMOTES): %:
	git fetch "$@"
	@remote="$@/master";                                  \
	path="$@";                                            \
	echo "subtree merging into $$path";                   \
	if [ ! -d "$$path" ]; then                            \
	  git merge -s ours --no-commit "$$remote";           \
	  git read-tree --prefix="$$path" -u "$$remote";      \
	  git commit -m "Initialize subtree at '$$path'";     \
	fi;                                                   \
	git merge -s recursive -X subtree="$$path"            \
	  -m "Merge subtree '$$remote'" "$$remote"

%.html: %.rst
	rst2html $? $@

clean:
	rm -f *.html
