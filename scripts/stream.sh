#!/bin/bash

case "$1" in
     catradio) url="http://www.catradio.cat/directes/catradio_http.m3u"   ;;
      catinfo) url="http://www.catradio.cat/directes/catinfo_http.m3u"    ;;
    catmusica) url="http://www.catmusica.cat/directes/catmusica_http.m3u" ;;
         icat) url="http://www.catradio.cat/directes/icat_http.m3u"       ;;
            *) echo "$1 not recognized"; exit 1                           ;;
esac

exec mpv --really-quiet --playlist="$url"

