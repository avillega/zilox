# Adding a instruction
To add a instruction we first need to add the opCode in the chunks.zig file, 
not all instruction require a 1 to 1 mapping with opCode but some do.

Then modify the parser table to know how to compile the specific tokens that are
part of the instruction.
