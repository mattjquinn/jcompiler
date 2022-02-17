#!/bin/bash

set -ex

# Install LLVM and Clang
sudo apt update
sudo apt install -y llvm-10 clang-10
clang-10 --version
