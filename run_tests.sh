#!/bin/bash

bash make_build.sh
cd build
ninja check
cd ..