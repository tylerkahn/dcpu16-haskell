{-# LANGUAGE NoMonomorphismRestriction #-}

module DCPU16.Assembler (assemble, assembleFile) where

import Prelude hiding (lookup)
import Text.Parsec hiding (label)
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Data.Word hiding (Word)
import Control.Applicative ((<$>))
import Data.Char (toUpper, toLower, ord)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Map (Map, lookup, empty, insert, union, fromList)
import Data.Bits

capitalize [] = []
capitalize (c:cs) = (:) (toUpper c) (map toLower cs)

upperCase = map (map toUpper . show)

type Program = [Statement]

data Statement = Statement [Label] Directive
    deriving (Show, Eq)

data Directive = Dat [Data]
    | Operation Mnemonic
    | MonadicOperation MonadicMnemonic Operand
    | DyadicOperation DyadicMnemonic Operand Operand
    deriving (Show, Eq)

data Data = DataString String | DataWord Word16
    deriving (Show, Eq)

data DyadicMnemonic = NonBasic | Set | Add | Sub | Mul | Div | Mod
       | Shl | Shr | And | Bor | Xor | Ife | Ifn | Ifg | Ifb
    deriving (Show, Eq, Read, Enum)
data MonadicMnemonic = NonNonBasic | Jsr
    deriving (Show, Eq, Read, Enum)
data Mnemonic = Brk
    deriving (Show, Eq, Read, Enum)

data Operand = Register Reg
    | Memory MemorySynonym
    | MemoryOffset MemorySynonym Reg
    | MemoryRegister Reg
    | Pop | Push | Peek
    | Identifier Label
    | Literal Word16
    deriving (Show, Eq, Read)

data MemorySynonym = LiteralMemory Word16 | LabelMemory Label
    deriving (Show, Eq, Read)

data Label = Label String
    deriving (Show, Eq, Read, Ord)

data Reg = A | B | C | X | Y | Z | I | J | PC | O | SP
    deriving (Show, Eq, Enum, Read)

dyadicMnemonics = upperCase [Set ..]
monadicMnemonics = upperCase [Jsr ..]
mnemonics = upperCase [Brk ..]
stackOperands = upperCase [Pop, Push, Peek]
registers = upperCase [A ..]

-- Parser

lexer = P.makeTokenParser $ emptyDef {
            P.identStart = letter <|> oneOf "._",
            P.commentLine = ";",
            P.reservedNames = dyadicMnemonics ++ monadicMnemonics
                ++ mnemonics ++ stackOperands ++ registers ++ ["DAT"],
            P.caseSensitive = False
        }

symbol = P.symbol lexer
identifier = P.identifier lexer
brackets = P.brackets lexer
int16 = fromIntegral <$> P.integer lexer
stringLiteral = P.stringLiteral lexer
whiteSpace = P.whiteSpace lexer
commaSep1 = P.commaSep1 lexer

matchString = try . symbol

stackOperand = read . capitalize <$> choice (map symbol stackOperands)
mnemonic = read . capitalize <$> choice (map matchString mnemonics)
dyadicMnemonic = read . capitalize <$> choice (map matchString dyadicMnemonics)
monadicMnemonic = read . capitalize <$> choice (map matchString monadicMnemonics)
label = Label <$> (char ':' >> identifier <?> "label")
labelName = Label <$> identifier
register = read <$> choice (map matchString registers) <?> "register"
operand = Identifier <$> labelName
        <|> Register <$> register
        <|> stackOperand
        <|> Literal <$> int16
        <|> MemoryRegister <$> try (brackets register)
        <|> Memory <$> try (brackets memorySynonym)
        <|> brackets (do {  mem <- memorySynonym; symbol "+";
                            r <- register;
                            return $ MemoryOffset mem r;})

memorySynonym = LabelMemory <$> labelName
    <|> LiteralMemory <$> int16

memoryDirective = Dat <$> (symbol "DAT" >> commaSep1 (dataString <|> dataWord))
    where
        dataString = DataString <$> stringLiteral
        dataWord = DataWord <$> int16

directive = memoryDirective <|>
    choice [dyadicOperation, monadicOperation, operation]
    where
        dyadicOperation = do {
            m <- dyadicMnemonic;
            op1 <- operand; symbol ",";
            op2 <- operand;
            return $ DyadicOperation m op1 op2;}
        monadicOperation = do {
            m <- monadicMnemonic;
            op <- operand;
            return $ MonadicOperation m op;}
        operation = Operation <$> mnemonic

statement = do
    labels <- many label
    d <- directive
    return $ Statement labels d

program = do{ whiteSpace; stmts <- many statement; eof; return stmts;}

-- Code Gen
-- TODO: Error handling for labels that aren't defined
-- TODO: Error handling for MemoryRegister with special registers.
--         Consider lifting error into parser.

directiveCodeGen :: Map Label Word16 -> Directive -> [Word16]
directiveCodeGen _ (Dat ds) = concatMap dataCodeGen ds
directiveCodeGen _ (Operation m) = [fromIntegral (fromEnum m) `shiftL` 10]
directiveCodeGen t (MonadicOperation m a) = catMaybes $ (Just $
    (a' `shiftL` 10)
    .|. fromIntegral (fromEnum m) `shiftL` 4)
    : [a'']
    where
        (a', a'') = operandCodeGen t a
directiveCodeGen t (DyadicOperation m a b) = catMaybes $ (Just $
    (b' `shiftL` 10)
    .|. (a' `shiftL` 4)
    .|. fromIntegral (fromEnum m))
    : [a'', b'']
    where
        (a', a'') = operandCodeGen t a
        (b', b'') = operandCodeGen t b

dataCodeGen :: Data -> [Word16]
dataCodeGen (DataString s) = map (fromIntegral . ord) s
dataCodeGen (DataWord w) = [w]

operandCodeGen :: Map Label Word16 -> Operand -> (Word16, Maybe Word16)
operandCodeGen _ (Register SP) = (0x1b, Nothing)
operandCodeGen _ (Register PC) = (0x1c, Nothing)
operandCodeGen _ (Register O)  = (0x1d, Nothing)
operandCodeGen _ (Register r)  = (fromIntegral $ fromEnum r, Nothing)
operandCodeGen _ (Memory (LiteralMemory addr)) = (0x1e, Just addr)
operandCodeGen t (Memory (LabelMemory l)) = (0x1e, Just $ resolveLabelAddress t l)
operandCodeGen _ (MemoryOffset (LiteralMemory addr) r) =
    (fromIntegral $ fromEnum r + 0x10, Just addr)
operandCodeGen t (MemoryOffset (LabelMemory l) r) =
    (fromIntegral $ fromEnum r + 0x10, Just $ resolveLabelAddress t l)
operandCodeGen _ (MemoryRegister r) = (fromIntegral (fromEnum r + 0x8), Nothing)
operandCodeGen _ Pop = (0x18, Nothing)
operandCodeGen _ Peek = (0x19, Nothing)
operandCodeGen _ Push = (0x1a, Nothing)
operandCodeGen t (Identifier l) = (0x1f, Just $ resolveLabelAddress t l)
operandCodeGen _ (Literal num) = if num < 32 then (num + 0x20, Nothing) else (0x1f, Just num)

resolveLabelAddress t l = fromMaybe 0 (lookup l t)

firstPass stmts = fst $ last $ scanl firstPass' (empty, 0) stmts

firstPass' :: (Map Label Word16, Int) -> Statement -> (Map Label Word16, Int)
firstPass' (m, n) (Statement ls d) = (m', n')
    where
        m' = fromList (zip ls $ repeat (fromIntegral n)) `union` m
        n' = n + length (directiveCodeGen empty d)

secondPass stmts = concatMap (\(Statement _ d) -> directiveCodeGen m d) stmts
    where m = firstPass stmts

assemble s fileName = secondPass <$> parse program fileName s
assembleFile fileName = fmap secondPass <$> parseFromFile program fileName
