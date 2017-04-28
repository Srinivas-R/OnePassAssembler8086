# OnePassAssembler8086
One pass assembler for 8086

Primitive one pass assembler for 8086.
This handles 5 addressing modes. Register, Register Relative, Direct, Indexed, Based Indexed.

To compile, 
./exec1.sh
To run, 
./single_ASM /path/to/input/file

eg. ./single_ASM prog1.asm
    ./single_ASM prog2.asm
    ./single_ASM prog3.asm

Forward referencing is handled.
