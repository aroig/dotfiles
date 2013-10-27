MAKE=emacs -Q --batch -f batch-byte-compile

ELCS=ispell-multi.elc outline-magic.elc rst.elc sensitive-mode.elc sr-speedbar.elc vala-mode.elc vim-modeline.elc winring.elc

.PHONY: all

%.elc: %.el
	$(MAKE) $< 

all: $(ELCS)

clean:
	rm -rf $(ELCS)
