#!/bin/bash
set -o errexit

# Back up an encrypted copy of priv
private_bak=$AB2_VAR_DIR/bak/priv/priv-$(date +%Y-%m-%d).tar.gpg
private_link=$AB2_VAR_DIR/bak/priv/priv.tar.xz.gpg
privdir="$AB2_PRIV_DIR"

# encrypt with AES256. Stretch key with 6.5e7 rounds of SHA512.
GPG_FLAGS="--cipher-algo AES256 --s2k-digest-algo SHA512 --s2k-mode 3 --s2k-count 65000000"

# make the backup package
tar -c --dereference --xz -C "$privdir" safe | gpg $GPG_FLAGS -c > "$private_bak"

# and symlink it
ln -sfv ./$(basename "$private_bak") "$private_link"

# email-me a copy
# echo "Ornitorinc backup." | mail -s "ornitorinc backup" -a "$private_bak" abdo

