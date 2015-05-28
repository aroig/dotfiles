EMACS  := emacs
EFLAGS := -Q --batch -f batch-byte-compile

ELCS=cmake-mode.elc ispell-multi.elc outline-magic.elc rst.elc sensitive-mode.elc \
     sr-speedbar.elc vim-modeline.elc winring.elc sudo.elc qml-mode.elc           \

DOWNLOAD_URLS=sr-speedbar.el::http://www.emacswiki.org/emacs/download/sr-speedbar.el \
              cmake-mode.el::https://raw.githubusercontent.com/Kitware/CMake/master/Auxiliary/cmake-mode.el \
              rst.el::http://docutils.sourceforge.net/tools/editors/emacs/rst.el \
              qml-mode.el::http://www.emacswiki.org/emacs/download/qml-mode.el

.PHONY: all download

%.elc: %.el
	$(EMACS) $(EFLAGS) $< 

all: $(ELCS)

download:
	for url in $(DOWNLOAD_URLS); do          \
	  curl "$${url##*::}" > "$${url%%::*}";  \
	done        

clean:
	rm -rf *.elc
