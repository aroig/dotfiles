#!/usr/bin/bash

for arg in "$@"; do
    echo "$arg"
done | awesome-client
