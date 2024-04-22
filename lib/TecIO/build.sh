#!/bin/bash -

rm -rf bin build && mkdir -p build
cd build
cmake ../teciosrc/
make