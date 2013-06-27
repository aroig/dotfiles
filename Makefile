MAKE=emacs -Q --batch -f batch-byte-compile

EMACS_LISP=emacs-lisp
MODES=coffee-mode git-modes lua-mode markdown-mode multi-mode pkgbuild-mode rainbow-mode sage-mode yaml-mode
PACKAGES=ac-math ibuffer-vc o-blog powerline yasnippet
THEMES=zenburn

# All packages with relative path
ALLPKG=$(patsubst %,modes/%,$(MODES)) $(patsubst %,packages/%,$(PACKAGES)) $(patsubst %,themes/%,$(THEMES)) hacks abdo

# All compilable packages with relative path
COMPPKG=$(patsubst %,modes/%,$(MODES)) $(patsubst %,packages/%,$(PACKAGES)) abdo

# Packages with git remote to track
GITPKG=$(patsubst %,modes/%,$(MODES)) $(patsubst %,packages/%,$(PACKAGES)) $(patsubst %,themes/%,$(THEMES))

.PHONY: all links $(COMPPKG)

all: $(COMPPKG)

# packages/ac-math:
#	cd $@; $(MAKE) *.el

packages/ibuffer-vc:
	cd $@; $(MAKE) *.el

packages/o-blog:
	cd $@; $(MAKE) *.el

# packages/powerline:
#	cd $@; $(MAKE) *.el

packages/yasnippet:
	cd $@; $(MAKE) yasnippet.el

modes/coffee-mode:
	cd $@; $(MAKE) *.el

modes/git-modes:
	cd $@; make

modes/lua-mode:
	cd $@; make

modes/markdown-mode:
	cd $@; $(MAKE) *.el

# modes/multi-mode:
#	cd $@; $(MAKE) *.el

# modes/pkgbuild-mode:
#	cd $@; $(MAKE) *.el

# modes/sage-mode:
#	cd $@/emacs; $(MAKE) *.el

modes/yaml-mode:
	cd $@; make

links:
	rm -f $(EMACS_LISP)/*
	@for pkg in $(ALLPKG); do ln -sfv ../$$pkg $(EMACS_LISP)/$$(basename $$pkg); done

pull:
	@for pkg in $(GITPKG); do echo $$pkg; cd ./$$pkg; git pull; cd -; done
