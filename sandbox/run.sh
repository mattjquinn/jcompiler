#!/bin/bash

# Use this script to experimentally compile/assemble various constructs
# to see how the cross-compiler compiles them.

export ARM_LINARO_CROSS_COMPILER_PATH="/opt/gcc-linaro-7.4.1-2019.02-x86_64_arm-linux-gnueabihf"

# This outputs the ARM for the common definition include.
$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-gcc \
        -v \
        -static \
        -S -o jarm.s \
        -c jarm.c

# This outputs the ARM for the main function.
$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-gcc \
        -v \
        -static \
        -S -o main.s \
        -c main.c

# This compiles them together and produces an output that can be invoked
# to ensure that together they are functioning correctly.
$ARM_LINARO_CROSS_COMPILER_PATH/bin/arm-linux-gnueabihf-gcc -v -static main.c jarm.c -o main -lm

