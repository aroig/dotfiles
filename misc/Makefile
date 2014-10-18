EMACS  := emacs
EFLAGS := -Q --batch -f batch-byte-compile

ELCS=cmake-mode.elc ispell-multi.elc outline-magic.elc rst.elc sensitive-mode.elc \
     sr-speedbar.elc vim-modeline.elc winring.elc sudo.elc          \
	 clang-format.elc 

.PHONY: all

%.elc: %.el
	$(EMACS) $(EFLAGS) $< 

all: $(ELCS)

clean:
	rm -rf *.elc
