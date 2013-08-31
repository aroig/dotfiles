
.PHONY: all

all: zathurarc.galois zathurarc.grothendieck zathurarc.hodge

zathurarc.galois: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set border-width\s*0/set border-width            4/' $@

zathurarc.grothendieck: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set border-width\s*0/set border-width            4/' $@

zathurarc.hodge: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set border-width\s*0/set border-width            4/' $@
