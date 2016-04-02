# ----------------------------------------------------------------------- #
# Abdó Roig-Maranges <abdo.roig@gmail.com>                                #
#                                                                         #
# Makefile to compile my dotfiles directory for github. Merges changes    #
# from several locar repos on my machine into a single dotfiles repo      #
# using the git merge -s subtree.                                         #
# ----------------------------------------------------------------------- #

REMOTES=awesome ranger shell spacemacs systemd vim zathura

# shell settings
SHELL       := /usr/bin/bash
.SHELLFLAGS := -e -u -c

.ONESHELL:

# So we can use $$(variable) on the prerequisites, that expand at matching time.
.SECONDEXPANSION:

.PHONY: all merge $(REMOTES)

all: merge

merge: $(REMOTES)

$(REMOTES): %:
	@git fetch "$@"
	remote="$@/master"
	path="$@"
	echo "subtree merging into '$$path'"
	if [ ! -d "$$path" ]; then
	    git merge -s ours --no-commit "$$remote"
	    git read-tree --prefix="$$path" -u "$$remote"
	    git commit -m "Initialize subtree at '$$path'"
	fi
	git merge -s recursive -X subtree="$$path" -m "Merge subtree '$$remote'" "$$remote"
