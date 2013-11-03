EMACS  := emacs
EFLAGS := -Q --batch -f batch-byte-compile

ELCS=org-hacks.elc rcirc-hacks.elc

.PHONY: all

%.elc: %.el
	$(EMACS) $(EFLAGS) $< 

all: $(ELCS)

clean:
	rm -rf *.elc
