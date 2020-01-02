stack run examples/good/$1.lat > $1.s
nasm -f elf32 $1.s
gcc -o $1 -m32 $1.o lib/runtime.o
./$1
