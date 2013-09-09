
.PHONY: all

all: zathurarc.galois zathurarc.grothendieck zathurarc.hodge

zathurarc.galois: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set statusbar-v-padding.*/set statusbar-v-padding     2/'  $@

zathurarc.grothendieck: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set statusbar-v-padding.*/set statusbar-v-padding     2/'  $@

zathurarc.hodge: zathurarc.in
	cp zathurarc.in $@
	sed -i 's/set statusbar-v-padding.*/set statusbar-v-padding     2/'  $@
