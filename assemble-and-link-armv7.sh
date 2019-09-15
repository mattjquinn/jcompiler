#!/bin/sh

# These commands below were taken from the output of running:
#
#    $ arm-linux-gnueabihf-gcc -v -static <in.c> -o <out>
#
# where <in.c> was a simple C program that included "stdio.h"
# Obviously this script is not very portable, and really should
# use a portable toolchain rather than one provided by the Debian
# repositories, but it's not a primary concern at the moment.

# This can be upgraded to 9 once
# Travis provides 19.04 Ubuntu machines (use the
# `gcc-9-arm-linux-gnueabihf` apt package)
export GCC_VERSION=8

arm-linux-gnueabihf-as \
        -march=armv7-a \
        -mfloat-abi=hard \
        -mfpu=vfpv3-d16 \
        -meabi=5 \
        -o $1.o \
        $1

arm-linux-gnueabihf-ld \
        -plugin /usr/lib/gcc-cross/arm-linux-gnueabihf/$GCC_VERSION/liblto_plugin.so \
        -plugin-opt=/usr/lib/gcc-cross/arm-linux-gnueabihf/$GCC_VERSION/lto-wrapper \
        -plugin-opt=-fresolution=/tmp/ccGcImIe.res \
        -plugin-opt=-pass-through=-lgcc \
        -plugin-opt=-pass-through=-lgcc_eh \
        -plugin-opt=-pass-through=-lc \
        --sysroot=/ \
        --build-id \
        -Bstatic \
        -X \
        --hash-style=gnu \
        -m armelf_linux_eabi \
        -o $2 \
        /usr/arm-linux-gnueabihf/lib/crt1.o \
        /usr/arm-linux-gnueabihf/lib/crti.o \
        /usr/lib/gcc-cross/arm-linux-gnueabihf/$GCC_VERSION/crtbeginT.o \
        -L/usr/lib/gcc-cross/arm-linux-gnueabihf/$GCC_VERSION \
        -L/usr/arm-linux-gnueabihf/lib \
        -L/usr/lib/arm-linux-gnueabihf \
        $1.o \
        --start-group -lgcc -lgcc_eh -lc --end-group \
        /usr/lib/gcc-cross/arm-linux-gnueabihf/$GCC_VERSION/crtend.o \
        /usr/arm-linux-gnueabihf/lib/crtn.o

