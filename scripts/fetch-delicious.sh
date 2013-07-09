#!/bin/bash

# url and paths
url=https://api.del.icio.us/v1/posts/all
dest=$HOME/var/delicious/$(date +%Y-%m-%d)-delicious.xml
link=$HOME/var/delicious/delicious.xml

# user and password in netrc
curl -S -s -L -n "$url" > "$dest"
ln -sf $dest $link
