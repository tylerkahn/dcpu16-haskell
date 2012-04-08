## DCPU-16 in Haskell

### An emulator and assembler for Notch's DCPU-16 


## What's different

### Storage Directives
`.word <integer literal>` initializes a word of memory with the specified value

`.stringz <string literal>` initializes a contiguous block of memory with the value of each character (one character per word) and a null-terminator at the end (word with value 0x0000).

### Labels as Literals
Labels can be used wherever literal integers can (except in the .word directive).

    SET X, answer       ; X = 4
    SET Y, [answer]     ; Y = 42
    :answer .word 42

### Halt Instruction

    SET X, 42   ; 7c31 2a
    HLT         ; 0000

Because I'm using a pure state machine to model DCPU execution, I need some way to 'halt' the program in order to display its final state. So to that end I've added the HLT instruction which sets the HLT register to 1, thereby terminating execution and yielding the final state. Because HLT is an operation with no operands, it works out nicely that if you make it a Non-Non-Basic opcode with an 'a' value of 0x0 as in (aaaaaa0000000000), the whole instruction takes on the value 0x0000.

This has the nice property such that when the cpu attempts to execute a word of uninitialized memory, it will halt.


## Todo

- `SET X, [PC]` is an invalid operation. Currently if you write this the assembler will generate a junk value. There's the option to make this a semantic error, but it's probably best that it be a syntax error.

- Referencing undefined labels doesn't throw an error. It will simply give you a 0 back for that label. This is a semantic error that needs to be added.

- Count cycles.

- Add video memory extension. Will need to have some actions be in the IO monad and therefore I'll need to make use of StateT instead of plain-old State.

- Probably will change HLT to BRK at some point if that gains more traction. I like HLT because it's reminiscent of the halting problem.
