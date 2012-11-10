MAKE=emacs -Q --batch -f batch-byte-compile

ELCS=$(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: all

%.elc: %.el
	$(MAKE) $< 

all: $(ELCS)

clean:
	rm -rf $(ELCS)
