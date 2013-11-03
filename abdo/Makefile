EMACS  := emacs
EFLAGS := -Q --batch -f batch-byte-compile

ELCS=$(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: all

%.elc: %.el
	$(EMACS) $(EFLAGS) $< 

all: $(ELCS)

clean:
	rm -rf $(ELCS)
