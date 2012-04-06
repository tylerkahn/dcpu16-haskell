{-# LANGUAGE TupleSections #-}

module DCPU16.Emulator (DCPUState (..), loadProgram,
    startingDCPUState, runProgram, execProgram) where

import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

access :: Integral a => a -> [b] -> b
access = flip (!!) . fromIntegral

data DCPUState = DCPUState { ram :: [Word16], registerFile :: [Word16],
    sp :: Word16, pc :: Word16, o :: Word16, cycles :: Int, hlt :: Word16}
        deriving (Eq)

instance Show DCPUState where
    show (DCPUState ram regFile sp pc o cycles hlt) =
        concatMap (\(l, v) -> l ++ ": " ++ show v ++ " ") pairs  ++ "\n" where
            pairs = [("HLT", hlt), ("PC", pc), ("SP", sp), ("*SP", access sp ram), ("O", o)] ++
                zip ["A","B","C","X","Y","Z","I","J"] regFile

data Instruction = Instruction BasicOpcode Word8 Word8
    deriving (Eq, Show)

data CellAddr = RAM Word16 | Reg Word8 | SP | PC | O | Literal
    deriving (Eq, Show)

data BasicOpcode = NonBasic | SET | ADD | SUB | MUL | DIV | MOD
    | SHL | SHR | AND | BOR | XOR | IFE | IFN | IFG | IFB
        deriving (Show, Eq, Enum)

data NonBasicOpcode = Reserved | JSR | HLT
    deriving (Show, Eq, Enum)

startingDCPUState = DCPUState (replicate 0x10000 0) (replicate 8 0) 0xffff 0 0 0 0

loadProgram :: [Word16] -> DCPUState -> DCPUState
loadProgram p cpu = cpu { ram = ram' } where
    ram' = p ++ replicate (0x10000 - length p) 0

decodeWord :: Integral a => Word16 -> (BasicOpcode, a, a)
decodeWord x = (toEnum $ fromIntegral o, fromIntegral a, fromIntegral b) where
    o = x .&. 0xf
    a = (x `shiftR` 4) .&. 0x3f
    b = (x `shiftR` 10) .&. 0x3f

currentWord :: DCPUState -> Word16
currentWord cpu = (!!) (ram cpu) $ fromIntegral (pc cpu)

nextWord :: State DCPUState Word16
nextWord = do
        modify (\cpu -> cpu { pc = 1 + pc cpu })
        gets currentWord
skip = void nextWord

push :: Word16 -> State DCPUState ()
push x = do
    sp' <- liftM (+ (-1)) (gets sp)
    modify (\cpu -> cpu {sp = sp', ram = replaceNth sp' x (ram cpu) })

incCyclesBy :: Int -> State DCPUState ()
incCyclesBy n = do
        cpu <- get
        put $ cpu { cycles = n + cycles cpu }

operandAction :: Integral a => a -> State DCPUState (Word16, CellAddr)
operandAction a
    | a <= 0x7 = do
            val <- (access a . registerFile) `fmap` get
            return (val, Reg $ fromIntegral a)
    | a <= 0x0f = do
            rVal <- (access (a - 0x8) . registerFile) `fmap` get
            val <- (access rVal . ram) `fmap` get
            return (val, RAM rVal)
    | a <= 0x17 = do
            rVal <- (access (a - 0x10) . registerFile) `fmap` get
            nWord <- nextWord
            let addr = rVal + nWord
            val <- (access addr . ram) `fmap` get
            return (val, RAM addr)
    | a == 0x18 = do
            cpu <- get
            put $ cpu { sp = 1 + sp cpu }
            let val = access (sp cpu) (ram cpu)
            return (val, RAM $ sp cpu)
    | a == 0x19 = do
            cpu <- get
            let val = access (sp cpu) (ram cpu)
            return (val, RAM (sp cpu))
    | a == 0x1a = do
            cpu <- get
            let sp' = sp cpu - 1
            put $ cpu { sp = sp' }
            val <- (access sp' . ram) `fmap` get
            return (val, RAM sp')
    | a == 0x1b = liftM ((, SP) . sp) get
    | a == 0x1c = liftM ((, PC) . pc) get
    | a == 0x1d = liftM ((, O) . o) get
    | a == 0x1e = do
            nWord <- nextWord
            val <- (access nWord . ram) `fmap` get
            return (val, RAM nWord)
    | a == 0x1f = liftM (, Literal) nextWord
    | otherwise = return (fromIntegral (a - 32), Literal)

skipNextInstruction = do
    cpu <- get
    (_, arg1, arg2) <- fmap decodeWord nextWord
    operandAction arg1 >> operandAction arg2
    cpu' <- get
    put $ cpu { pc = (pc cpu') }

set :: CellAddr -> Word16 -> State DCPUState ()
set (Reg a) b = do
            cpu <- get
            let regFile' = replaceNth a b (registerFile cpu)
            put $ cpu {registerFile = regFile'}
set (RAM a) b = do
            cpu <- get
            let ram' = replaceNth a b (ram cpu)
            put $ cpu {ram = ram'}
set SP b = modify (\cpu -> cpu { sp = b })
set PC b = modify (\cpu -> cpu { pc = b - 1 })
set O b = modify (\cpu -> cpu { o = b })
set Literal b = return ()

instructionAction' :: BasicOpcode -> Word32 -> Word32 -> CellAddr -> State DCPUState ()
instructionAction' op a b addr
    | op == SET = set addr (fromIntegral b)
    | op == ADD = do
        let sum = a + b
        set O (if testBit sum 16 then 0x1 else 0x0)
        set addr $ fromIntegral sum
    | op == SUB = do
        let diff = a - b
        set O (if testBit diff 16 then 0xffff else 0x0)
        set addr $ fromIntegral diff
    | op == MUL = do
        let prod = a * b
        set O $ fromIntegral (prod `shiftR` 16)
        set addr $ fromIntegral prod
    | op == DIV = case b of 0 -> set addr 0 >> set O 0
                            _ -> do
                                let quotient = div a b
                                set O $ fromIntegral ((a `shiftL` 16) `div` b)
                                set addr $ fromIntegral quotient
    | op == MOD = case b of 0 -> set addr 0
                            _ -> set addr $ fromIntegral $ mod a b
    | op == SHL = do
        let shifted = a `shiftL` fromIntegral b
        set O $ fromIntegral (shifted `shiftR` 16)
        set addr $ fromIntegral shifted
    | op == SHR = do
        let shifted = a `shiftR` fromIntegral b
        set O $ fromIntegral ((a `shiftL` 16) `shiftR` fromIntegral b)
        set addr $ fromIntegral shifted
    | op == AND = set addr $ fromIntegral $ a .&. b
    | op == BOR = set addr $ fromIntegral $ a .|. b
    | op == XOR = set addr $ fromIntegral $ a `xor` b
    | op == IFE = unless (a == b) $ skipNextInstruction
    | op == IFN = unless (a /= b) $ skipNextInstruction
    | op == IFG = unless (a > b) $ skipNextInstruction
    | op == IFB = unless ((a .&. b) /= 0) $ skipNextInstruction

instructionAction :: Integral a => BasicOpcode -> a -> a -> State DCPUState ()
instructionAction NonBasic o a = do
    (a', addr) <- operandAction a
    nonBasicInstructionAction (toEnum $ fromIntegral o) (fromIntegral a') addr
instructionAction o a b = do
    (a', addr) <- operandAction a
    (b', _) <- operandAction b
    instructionAction' o (fromIntegral a') (fromIntegral b') addr

nonBasicInstructionAction :: Integral a => NonBasicOpcode -> a -> CellAddr -> State DCPUState ()
nonBasicInstructionAction op a addr
    | op == JSR = do
        pc' <- gets pc
        push $ pc' + 1
        modify (\cpu -> cpu { pc = fromIntegral (a - 1) })
    | op == HLT = modify (\cpu -> cpu { hlt = 0x1 })

pulse :: State DCPUState DCPUState
pulse = do
    (op, arg1, arg2) <- fmap decodeWord $ gets currentWord
    instructionAction op arg1 arg2
    skip >> get

runUntil :: Monad m => (a -> Bool) -> StateT a m b -> a -> m [(b, a)]
runUntil p st i = if p i then return [] else do
        k@(val, i') <- runStateT st i
        liftM (return k ++) $ runUntil p st i'

runProgram p = map fst $ runIdentity $
        runUntil (\cpu -> hlt cpu > 0) pulse (loadProgram p startingDCPUState)
execProgram p = last $ runProgram p
