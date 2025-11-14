#!/bin/bash
# Script to run the intersection visualization test

cd "$(dirname "$0")"

echo "Building test..."
opam exec -- dune build test/test_intersection.exe

if [ $? -eq 0 ]; then
    echo "Running intersection visualization..."
    opam exec -- dune exec test/test_intersection.exe
else
    echo "Build failed!"
    exit 1
fi

