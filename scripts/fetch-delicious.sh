#!/bin/bash

# url and paths
url="https://api.del.icio.us/v1/posts/all"

tgtdir="$HOME/var/delicious"
dest="$tgtdir/delicious.xml"

# user and password in netrc
curl -S -s -L -n "$url" > "$dest" || exit 1

git --git-dir="$tgtdir/.git" --work-tree="$tgtdir" add -A . && \
git --git-dir="$tgtdir/.git" --work-tree="$tgtdir" commit -m 'updated bookmarks'
exit 0
