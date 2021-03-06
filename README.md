## DCPU-16 in Haskell

### An emulator and assembler for Notch's DCPU-16 


## What's supported

### Storage Directives
`DAT` is the de-facto way to do this.

`DAT <string literal | int literal>` initializes a contiguous block of memory with character values or literal values.

Example:

    :data DAT "a string literal followed by two integer literals", 42, 0x30

### Labels as Literals
Labels can be used wherever literal integers can (except in a storage directive).

    SET X, answer       ; X = 5
    SET J, 1
    SET Y, [answer+J]   ; Y = 66
    :answer DAT 42, 0x42

### Break Instruction
`BRK` is the de-facto way to halt the program execution.

    SET X, 42   ; 7c31 2a
    BRK         ; 0000

## Todo

- `SET X, [PC]` is an invalid operation. Currently if you write this the assembler will generate a junk value. There's the option to make this a semantic error, but it's probably best that it be a syntax error.

- Referencing undefined labels doesn't throw an error. It will simply give you a 0 back for that label. This is a semantic error that needs to be added.

- Count cycles.

- Add video memory extension. Will need to have some actions be in the IO monad and therefore I'll need to make use of StateT instead of plain-old State.

- Probably will change HLT to BRK at some point if that gains more traction. I like HLT because it's reminiscent of the halting problem.

- Add support for basic arithmetic expressions wherever literals can be used.

- Add support for using labels in storage directives (allowing for static computation of the size of a code block).
