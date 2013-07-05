
all: zathurarc.galois zathurarc.grothendieck zathurarc.hodge

zathurarc.galois:
	cp zathurarc $@
	sed -i 's/set border-width\s*0/set border-width            0/' $@

zathurarc.grothendieck:
	cp zathurarc $@
	sed -i 's/set border-width\s*0/set border-width            5/' $@

zathurarc.hodge:
	cp zathurarc $@
	sed -i 's/set border-width\s*0/set border-width            5/' $@
