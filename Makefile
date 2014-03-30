
.PHONY: update

update:
	update-desktop-database "$(HOME)/.local/share/applications"
	update-mime-database    "$(HOME)/.local/share/mime"
