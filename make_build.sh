#!/bin/bash

# export LDFLAGS="-L/opt/homebrew/opt/llvm/lib/unwind -lunwind"
# export LDFLAGS="-L/opt/homebrew/opt/llvm/lib/c++ -L/opt/homebrew/opt/llvm/lib/unwind -lunwind"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib/clang/19 -L/opt/homebrew/opt/llvm/lib/c++ -L/opt/homebrew/opt/llvm/lib/unwind -lunwind"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

rm -rf build
mkdir build
cmake -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER=/opt/homebrew/opt/llvm/bin/clang -DCMAKE_CXX_COMPILER=/opt/homebrew/opt/llvm/bin/clang++ -S . -DCMAKE_CXX_FLAGS="-fno-rtti" -B build
cmake --build ./build
