{-# LANGUAGE QuasiQuotes #-}

import DCPU16.Emulator
import DCPU16.Assembler
import Data.String.Interpolation

notch'sProgramBin = [0x7c01,0x0030,0x7de1,0x1000,
    0x0020,0x7803,0x1000,0xc00d,0x7dc1,0x001a,
    0xa861,0x7c01,0x2000,0x2161,0x2000,0x8463,
    0x806d,0x7dc1,0x000d,0x9031,0x7c10,0x0018,
    0x7dc1,0x001a,0x9037,0x61c1,0x7dc1,0x001a]

-- uses custom HLT instruction instead of
-- looping forever
myProgramBin =      [0x7c01,0x0030,0x7de1,0x1000,
    0x0020,0x7803,0x1000,0xc00d,0x7dc1,0x001a,
    0xa861,0x7c01,0x2000,0x2161,0x2000,0x8463,
    0x806d,0x7dc1,0x000d,0x9031,0x7c10,0x0018,
    0x7dc1,0x001a,0x9037,0x61c1,0x0000,0x0000]


myProgram = [str|
        ; Try some basic stuff
                      SET Y, [answer]
                      SET A, 0x30              ; 7c01 0030
                      SET [0x1000], 0x20       ; 7de1 1000 0020
                      SUB A, [0x1000]          ; 7803 1000
                      IFN A, 0x10              ; c00d
                         SET PC, crash         ; 7dc1 001a [*]

        ; Do a loopy thing
                      SET I, 10                ; a861
                      SET A, 0x2000            ; 7c01 2000
        :loop         SET [0x2000+I], [A]      ; 2161 2000
                      SUB I, 1                 ; 8463
                      IFN I, 0                 ; 806d
                         SET PC, loop          ; 7dc1 000d [*]

        ; Call a subroutine
                      SET X, 0x4               ; 9031
                      JSR testsub              ; 7c10 0018 [*]
                      SET PC, crash            ; 7dc1 001a [*]

        :testsub      SHL X, 4                 ; 9037
                      SET PC, POP              ; 61c1


        ; Halt. X should now be 0x40 and Y should be 42
        ; if everything went right.
        :crash        HLT                      ; 0000
        :answer       .word 42                 ; 002a
        :question     .stringz "What do you get if you multiply six by nine?"
|]

assembleAndExecute p pName = fmap execProgram $ assemble p pName
assembleAndExecuteFile fName = (fmap execProgram) `fmap` assembleFile fName

main = print $ fmap execProgram $ assemble myProgram "myProgram"
