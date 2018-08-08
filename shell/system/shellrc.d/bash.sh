#!/bin/bash

if [ "$BASH_VERSION" ]; then
    # extended globbing
    shopt -s extglob
    shopt -s globstar
fi
