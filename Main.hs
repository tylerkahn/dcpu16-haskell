{-# LANGUAGE QuasiQuotes #-}

import DCPU16.Emulator
import DCPU16.Assembler
import Data.String.Interpolation

myProgram = [str|
        ; Try some basic stuff
                      SET Y, [word]
                      SET A, 0x30              ; 7c01 0030
                      SET [0x1000], 0x20       ; 7de1 1000 0020
                      SUB A, [0x1000]          ; 7803 1000
                      IFN A, 0x10              ; c00d
                         SET PC, crash         ; 7dc1 001a [*]

        ; Do a loopy thing
                      SET I, 10
                      SET A, 0x2000
        :.loop         SET [0x2000+I], [A]
                      SUB I, 1
                      IFN I, 0
                         SET PC, .loop

        ; Call a subroutine
                      SET X, 0x4
                      JSR testsub
                      SET PC, testsum

        :testsub      SHL X, 4
                      SET PC, POP

        :testsum      ADD J, 1
                      ADD Z, [word+J]
                      IFN J, 3
                        SET PC, testsum


        ; Break. X should now be 0x40 and Y should be 42
        ; Z should be 294 ('a' + 'b' + 'c')
        ; if everything went right.
        :crash        BRK                      ; 0000
        :word         DAT 42, "abc"
|]

assembleAndExecute p pName = fmap execProgram $ assemble p pName
assembleAndExecuteFile fName = (fmap execProgram) `fmap` assembleFile fName

main = print $ fmap execProgram $ assemble myProgram "myProgram"
