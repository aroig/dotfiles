
.PHONY: all

all: zathurarc.galois zathurarc.grothendieck zathurarc.hodge

zathurarc.galois: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set border-width.*/set border-width            0/'         $@
	sed -i 's/set statusbar-v-padding.*/set statusbar-v-padding     0/'  $@

zathurarc.grothendieck: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set border-width\s*0/set border-width            4/'       $@
	sed -i 's/set statusbar-v-padding.*/set statusbar-v-padding     2/'  $@

zathurarc.hodge: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set border-width\s*0/set border-width            4/'       $@
	sed -i 's/set statusbar-v-padding.*/set statusbar-v-padding     2/'  $@
