#!/bin/bash -

readonly DIR=$(pwd)

# Boost setup
[[ ! -f "boost" ]] && tar zxf boost.tar.gz
cd teciosrc
ln -sfn ../boost .
cd $DIR

# Build teciosrc
cd teciosrc
mkdir -p build
cd build
cmake ..
make -j
cd $DIR

