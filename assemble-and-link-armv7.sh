#!/bin/sh

# Arguments: ./run.sh <path_to_assembly_file> <name_of_output_executable>

# WARNING: be extremely careful when upgrading to newer Linaro versions. As a smoke check,
# write a simple main.c and use the gcc command below to compile it alongside the library include.
# Past upgrade attempts have resulted in "multiple definitions for ..." errors which couldn't be resolved.
export ARM_LINARO_CROSS_COMPILER_PATH="/opt/linaro/gcc-linaro-7.4.1-2019.02-x86_64_arm-linux-gnueabihf"

# These commands below were taken from the output of running:
#
#    $ $ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-gcc -v -static <in.c> jarm.c -o <out>
#
# where <in.c> was a simple C program that included "stdio.h" and called to i.e. jprint_int.

# Note: each output executable gets its own jarm.o because the test suite runs concurrently.

echo "Building $2_jarm.o..."
$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-gcc \
        -v \
        -static \
        -o $2_jarm.o \
        -c c_defns/jarm.c

echo "Building $1.o..."
$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-as \
        -march=armv7-a \
        -mfloat-abi=hard \
        -mfpu=vfpv3-d16 \
        -mthumb \
        -meabi=5 \
        -o $1.o \
        $1

echo "Linking $1.o with $2_jarm.o..."
$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-ld \
        -plugin $ARM_LINARO_CROSS_COMPILER_PATH/libexec/gcc/arm-linux-gnueabihf/7.4.1/liblto_plugin.so \
        -plugin-opt=$ARM_LINARO_CROSS_COMPILER_PATH/libexec/gcc/arm-linux-gnueabihf/7.4.1/lto-wrapper \
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
        $ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf/7.4.1/crtbeginT.o \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf/7.4.1 \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/lib \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/lib \
        -L$ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/usr/lib \
        $1.o \
        $2_jarm.o \
        --start-group -lgcc -lgcc_eh -lc --end-group \
        $ARM_LINARO_CROSS_COMPILER_PATH/lib/gcc/arm-linux-gnueabihf/7.4.1/crtend.o \
        $ARM_LINARO_CROSS_COMPILER_PATH/arm-linux-gnueabihf/libc/usr/lib/crtn.o \
        -lm

