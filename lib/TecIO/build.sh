#!/bin/bash -

rm -rf bin build && mkdir -p build
cd build
cmake ../teciosrc/ -DCMAKE_BUILD_TYPE=RELEASE
make