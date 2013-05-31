#!/bin/bash

cert=~/etc/certs/vpn.upc.edu.der
host=vpn.upc.edu
user=abdo.roig
realm=Usuarios

cat ~/.netrc | grep "vpn\.upc\.edu.*abdo\.roig" | perl -n -e'/password\s*(.+)/ && print $1' | sudo ncsvc -L 0 -h $host -u $user -r $realm -f $cert
