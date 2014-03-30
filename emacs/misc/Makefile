EMACS  := emacs
EFLAGS := -Q --batch -f batch-byte-compile

ELCS=cmake-mode.elc ispell-multi.elc outline-magic.elc rst.elc sensitive-mode.elc \
     sr-speedbar.elc vala-mode.elc vim-modeline.elc winring.elc sudo.elc qml-mode.elc

.PHONY: all

%.elc: %.el
	$(EMACS) $(EFLAGS) $< 

all: $(ELCS)

clean:
	rm -rf *.elc
