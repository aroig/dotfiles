REMOTES=awesome emacs ranger shell systemd vim dwb zathura

SHELL := $(SHELL) -e

.PHONY: merge doc $(REMOTES)

merge: $(REMOTES)

doc: README.html

$(REMOTES): %:
	git fetch "$@"
	@if [ ! -d "$@" ]; then                               \
	  git merge -s ours --no-commit "$@/master";          \
	  git read-tree --prefix="$@" -u "$@/master";         \
	  git commit -m "Initialize subtree at '$@'";         \
	fi
	git merge -s recursive -X subtree="$@" -m "Merge subtree '$@/master'" "$@/master"

%.html: %.rst
	rst2html $? $@

clean:
	rm -f *.html
