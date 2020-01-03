#!/bin/bash
set -e
base=${1%.lat}
stack run $1 > $base.s
nasm -f elf32 $base.s
gcc -o $base -m32 $base.o lib/runtime.o
rm $base.o
./$base
