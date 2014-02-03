EMACS  := emacs
EFLAGS := -Q --batch -f batch-byte-compile

ELCS=ispell-multi.elc outline-magic.elc rst.elc sensitive-mode.elc \
     sr-speedbar.elc vala-mode.elc vim-modeline.elc winring.elc sudo.elc

.PHONY: all

%.elc: %.el
	$(EMACS) $(EFLAGS) $< 

all: $(ELCS)

clean:
	rm -rf *.elc
