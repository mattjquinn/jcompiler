#!/bin/sh

export ARM_LINARO_CROSS_COMPILER_PATH="/opt/gcc-linaro-7.5.0-2019.12-x86_64_arm-linux-gnueabihf"

# These commands below were taken from the output of running:
#
#    $ $ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-gcc -v -static <in.c> -o <out>
#
# where <in.c> was a simple C program that included "stdio.h".

$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-as \
        -march=armv7-a \
        -mfloat-abi=hard \
        -mfpu=vfpv3-d16 \
        -meabi=5 \
        -o $1.o \
        $1

$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-ld \
        -plugin $ARM_LINARO_CROSS_COMPILER_PATH/libexec/gcc/arm-linux-gnueabihf/7.5.0/liblto_plugin.so \
        -plugin-opt=$ARM_LINARO_CROSS_COMPILER_PATH/libexec/gcc/arm-linux-gnueabihf/7.5.0/lto-wrapper \
        -plugin-opt=-fresolution=/tmp/cc2CC7OQ.res \
        -plugin-opt=-pass-through=-lgcc \
        -plugin-opt=-pass-through=-lgcc_eh \
        -plugin-opt=-pass-through=-lc \
        --sysroot=$ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc \
        --build-id \
        -Bstatic \
        -X \
        --hash-style=gnu \
        -m armelf_linux_eabi \
        -o $2 \
        $ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/usr/lib/crt1.o \
        $ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/usr/lib/crti.o \
        $ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf/7.5.0/crtbeginT.o \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf/7.5.0 \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/lib \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/lib \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/usr/lib \
        $1.o \
        --start-group -lgcc -lgcc_eh -lc --end-group \
        $ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf/7.5.0/crtend.o \
        $ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/usr/lib/crtn.o

