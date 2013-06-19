MAKE=emacs -Q --batch -f batch-byte-compile

EMACS_LISP=emacs-lisp
MODES=coffee-mode git-modes lua-mode markdown-mode multi-mode pkgbuild-mode rainbow-mode sage-mode yaml-mode
PACKAGES=helm ibuffer-vc powerline
THEMES=zenburn

# All packages with relative path
ALLPKG=$(patsubst %,modes/%,$(MODES)) $(patsubst %,packages/%,$(PACKAGES)) $(patsubst %,themes/%,$(THEMES)) misc hacks abdo

# All compilable packages with relative path
COMPPKG=$(patsubst %,modes/%,$(MODES)) $(patsubst %,packages/%,$(PACKAGES)) misc abdo

# Packages with git remote to track
GITPKG=$(patsubst %,modes/%,$(MODES)) $(patsubst %,packages/%,$(PACKAGES)) $(patsubst %,themes/%,$(THEMES))

.PHONY: all links $(COMPPKG)

all: $(COMPPKG)

packages/auto-complete:
	cd $@; make byte-compile

$(COMPPKG): %:
	cd $@; if [ -f Makefile ]; then make; fi

links:
	rm -f $(EMACS_LISP)/*
	@for pkg in $(ALLPKG); do ln -sfv ../$$pkg $(EMACS_LISP)/$$(basename $$pkg); done

pull:
	@for pkg in $(GITPKG); do echo $$pkg; cd ./$$pkg; git pull; cd -; done
