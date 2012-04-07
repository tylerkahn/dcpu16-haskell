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

data DCPUState = DCPUState { mem :: [Word16], registerFile :: [Word16],
    sp :: Word16, pc :: Word16, o :: Word16, cycles :: Int, hlt :: Word16}
        deriving (Eq)

instance Show DCPUState where
    show (DCPUState mem regFile sp pc o cycles hlt) =
        concatMap (\(l, v) -> l ++ ": " ++ show v ++ " ") pairs  ++ "\n" where
            pairs = [("HLT", hlt), ("PC", pc), ("SP", sp), ("*SP", access sp mem), ("O", o)] ++
                zip ["A","B","C","X","Y","Z","I","J"] regFile

data Instruction = Instruction BasicOpcode Word8 Word8
    deriving (Eq, Show)

data CellAddr = Mem Word16 | Register Word8 | SP | PC | O | Literal
    deriving (Eq, Show)

data BasicOpcode = NonBasic | SET | ADD | SUB | MUL | DIV | MOD
    | SHL | SHR | AND | BOR | XOR | IFE | IFN | IFG | IFB
        deriving (Show, Eq, Enum)

data NonBasicOpcode = Reserved | JSR | HLT
    deriving (Show, Eq, Enum)

startingDCPUState = DCPUState (replicate 0x10000 0) (replicate 8 0) 0xffff 0 0 0 0

loadProgram :: [Word16] -> DCPUState -> DCPUState
loadProgram p cpu = cpu { mem = mem' } where
    mem' = p ++ replicate (0x10000 - length p) 0

decodeWord :: Integral a => Word16 -> (BasicOpcode, a, a)
decodeWord x = (toEnum $ fromIntegral o, fromIntegral a, fromIntegral b) where
    o = x .&. 0xf
    a = (x `shiftR` 4) .&. 0x3f
    b = (x `shiftR` 10) .&. 0x3f

currentWord :: DCPUState -> Word16
currentWord cpu = (!!) (mem cpu) $ fromIntegral (pc cpu)

nextWord :: State DCPUState Word16
nextWord = modify (\cpu -> cpu { pc = 1 + pc cpu }) >> gets currentWord
skip = void nextWord

skipNextInstruction = do
    cpu <- get
    (_, arg1, arg2) <- decodeWord `fmap` nextWord
    operandAction arg1 >> operandAction arg2
    modify (\cpu' -> cpu { pc = pc cpu' })


push :: Word16 -> State DCPUState ()
push x = do
    sp' <- liftM (+ (-1)) $ gets sp
    modify (\cpu -> cpu {sp = sp', mem = replaceNth sp' x (mem cpu) })

incCyclesBy :: Int -> State DCPUState ()
incCyclesBy n = modify (\cpu -> cpu { cycles = n + cycles cpu })

operandAction :: Integral a => a -> State DCPUState (Word16, CellAddr)
operandAction a
    | a <= 0x7 = do
            val <- gets $ access a . registerFile
            return (val, Register $ fromIntegral a)
    | a <= 0x0f = do
            rVal <- gets $ access (a - 0x8) . registerFile
            val <- gets $ access rVal . mem
            return (val, Mem rVal)
    | a <= 0x17 = do
            rVal <- gets $ access (a - 0x10) . registerFile
            nWord <- nextWord
            let addr = rVal + nWord
            val <- gets $ access addr . mem
            return (val, Mem addr)
    | a == 0x18 = do
            cpu <- get
            put $ cpu { sp = 1 + sp cpu }
            let val = access (sp cpu) (mem cpu)
            return (val, Mem $ sp cpu)
    | a == 0x19 = do
            cpu <- get
            let val = access (sp cpu) (mem cpu)
            return (val, Mem (sp cpu))
    | a == 0x1a = do
            modify (\cpu -> cpu { sp = sp cpu - 1})
            sp' <- gets sp
            val <- gets $ access sp' . mem
            return (val, Mem sp')
    | a == 0x1b = liftM ((, SP) . sp) get
    | a == 0x1c = liftM ((, PC) . pc) get
    | a == 0x1d = liftM ((, O) . o) get
    | a == 0x1e = do
            nWord <- nextWord
            val <- gets $ access nWord . mem
            return (val, Mem nWord)
    | a == 0x1f = liftM (, Literal) nextWord
    | otherwise = return (fromIntegral (a - 32), Literal)

set :: CellAddr -> Word16 -> State DCPUState ()
set (Register a) b = modify (\cpu -> cpu { registerFile = replaceNth a b (registerFile cpu) })
set (Mem a) b = modify (\cpu -> cpu { mem = replaceNth a b (mem cpu) })
set SP b = modify (\cpu -> cpu { sp = b })
set PC b = modify (\cpu -> cpu { pc = b - 1 })
set O b = modify (\cpu -> cpu { o = b })
set Literal b = return ()

instructionAction' :: BasicOpcode -> Word32 -> Word32 -> CellAddr -> State DCPUState ()
instructionAction' op a b addr
    | op == SET = set addr (fromIntegral b)
    | op == ADD = do
        set O (if testBit (a+b) 16 then 0x1 else 0x0)
        set addr $ fromIntegral (a+b)
    | op == SUB = do
        set O (if testBit (a-b) 16 then 0xffff else 0x0)
        set addr $ fromIntegral (a-b)
    | op == MUL = do
        set O $ fromIntegral ((a*b) `shiftR` 16)
        set addr $ fromIntegral (a*b)
    | op == DIV = case b of 0 -> set addr 0 >> set O 0
                            _ -> do
                                set O $ fromIntegral ((a `shiftL` 16) `div` b)
                                set addr $ fromIntegral $ div a b
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
    | op == IFE = unless (a == b) skipNextInstruction
    | op == IFN = unless (a /= b) skipNextInstruction
    | op == IFG = unless (a > b) skipNextInstruction
    | op == IFB = unless ((a .&. b) /= 0) skipNextInstruction

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
    (op, arg1, arg2) <- gets $ decodeWord . currentWord
    instructionAction op arg1 arg2
    skip >> get

runUntil :: Monad m => (a -> Bool) -> StateT a m b -> a -> m [(b, a)]
runUntil p st i = if p i then return [] else do
        k@(val, i') <- runStateT st i
        liftM (return k ++) $ runUntil p st i'

runProgram p = map fst $ runIdentity $
        runUntil (\cpu -> hlt cpu > 0) pulse (loadProgram p startingDCPUState)
execProgram p = last $ runProgram p
