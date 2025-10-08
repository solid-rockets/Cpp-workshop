To assemble/link your code, you will need to issue the following commands.
Comments on those are also provided.

    nasm -f elf64 <name>.asm
    gcc -no-pie <name>.o -o main

To call, type:

    ./main

IMPORTANT NOTE: 'gcc' looks for the 'main' symbol at linking time. If not present, it'll complain. Use the 'main' symbol as the entry point to your assembly code, at least on Linux.
