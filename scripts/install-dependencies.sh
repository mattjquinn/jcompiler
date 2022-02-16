#!/bin/bash

set -ex

# Install QEMU
sudo apt update
sudo apt install -y qemu-user-static
qemu-arm-static -version

# Install LLVM and Clang
sudo apt install -y llvm-10 clang-10
clang-10 --version

# Install Linaro cross-compiler
LINARO_CROSS_COMPILER_NAME="gcc-linaro-7.4.1-2019.02-x86_64_arm-linux-gnueabihf.tar.xz"
wget "https://releases.linaro.org/components/toolchain/binaries/7.4-2019.02/arm-linux-gnueabihf/$LINARO_CROSS_COMPILER_NAME"
sudo mkdir -p /opt/linaro
sudo tar xf $LINARO_CROSS_COMPILER_NAME -C /opt/linaro

# Install jsoftware interpreter
wget http://www.jsoftware.com/download/j901/install/j901_amd64.deb
sudo dpkg -i j901_amd64.deb
