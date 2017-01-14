#!/usr/bin/bash

cd "$1"
if output=$(git status --porcelain) && [ -z "$output" ]; then
    exit 0
else
    exit 1
fi
