#!/bin/bash
set -o errexit

# Dump calibre database before commit

repo="$1"
cd "$repo"
.githooks/metadata dump
                
