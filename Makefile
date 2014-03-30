
.PHONY: update

update:
	cp mimenew/* mime/packages/
	update-desktop-database "$(HOME)/.local/share/applications"
	update-mime-database    "$(HOME)/.local/share/mime"
