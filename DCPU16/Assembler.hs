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
import Data.Maybe (catMaybes)
import Data.Map (Map, lookup, empty, insert, union, fromList)
import Data.Bits

capitalize [] = []
capitalize (c:cs) = (:) (toUpper c) (map toLower cs)

upperCase = map (map toUpper . show)

type Program = [Statement]

data Statement = Statement [Label] Directive
    deriving (Show, Eq)

data Directive = StringZ String | Word Word16
    | Operation Mnemonic
    | MonadicOperation MonadicMnemonic Operand
    | DyadicOperation DyadicMnemonic Operand Operand
    deriving (Show, Eq)

data DyadicMnemonic = NonBasic | Set | Add | Sub | Mul | Div | Mod
       | Shl | Shr | And | Bor | Xor | Ife | Ifn | Ifg | Ifb
    deriving (Show, Eq, Read, Enum)
data MonadicMnemonic = NonNonBasic | Jsr
    deriving (Show, Eq, Read, Enum)
data Mnemonic = Hlt
    deriving (Show, Eq, Read, Enum)

data Operand = Register Reg
    | Memory Word16
    | MemoryOffset Word16 Reg
    | MemoryRegister Reg
    | Pop | Push | Peek
    | Identifier Label
    | Literal Word16
    deriving (Show, Eq, Read)

data Label = Label String
    deriving (Show, Eq, Read, Ord)

data Reg = A | B | C | X | Y | Z | I | J | PC | O | SP
    deriving (Show, Eq, Enum, Read)

dyadicMnemonics = upperCase [Set ..]
monadicMnemonics = upperCase [Jsr ..]
mnemonics = upperCase [Hlt ..]
stackOperands = upperCase [Pop, Push, Peek]
registers = upperCase [A ..]

-- Parser

lexer = P.makeTokenParser $ emptyDef {
            P.commentLine = ";",
            P.reservedNames = dyadicMnemonics ++ monadicMnemonics
                ++ mnemonics ++ stackOperands ++ registers,
            P.caseSensitive = False
        }

symbol = P.symbol lexer
identifier = P.identifier lexer
brackets = P.brackets lexer
word16 = P.natural lexer >>= return . fromIntegral
int16 = P.integer lexer >>= return . fromIntegral
stringLiteral = P.stringLiteral lexer
whiteSpace = P.whiteSpace lexer

matchString = try . symbol

stackOperand = read . capitalize <$> choice (map symbol stackOperands)
mnemonic = read . capitalize <$> choice (map matchString mnemonics)
dyadicMnemonic = read . capitalize <$> choice (map matchString dyadicMnemonics)
monadicMnemonic = read . capitalize <$> choice (map matchString monadicMnemonics)
label = Label <$> (char ':' >> identifier <?> "label")
register = read <$> choice (map matchString registers) <?> "register"
operand = Identifier . Label <$> identifier
        <|> Register <$> register
        <|> stackOperand
        <|> Literal <$> word16
        <|> MemoryRegister <$> try (brackets register)
        <|> Memory <$> try (brackets word16)
        <|> brackets (do {  mem <- word16;  symbol "+";
                            r <- register;
                            return $ MemoryOffset mem r;})

memoryDirective = char '.' >> (stringz <|> word)
    where
        stringz = StringZ <$> (symbol "stringz" >> stringLiteral)
        word = Word <$> (symbol "word" >> int16)

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
directiveCodeGen _ (StringZ s) = map (fromIntegral . ord) s
directiveCodeGen _ (Word x) = [x]
directiveCodeGen _ (Operation m) = [(fromIntegral $ fromEnum m) `shiftL` 10]
directiveCodeGen t (MonadicOperation m a) = catMaybes $ [Just $
    (a' `shiftL` 10)
    .|. (fromIntegral $ fromEnum m) `shiftL` 4]
    ++ [a'']
    where
        (a', a'') = operandCodeGen t a
directiveCodeGen t (DyadicOperation m a b) = catMaybes $ [Just $
    (b' `shiftL` 10)
    .|. (a' `shiftL` 4)
    .|. (fromIntegral $ fromEnum m)]
    ++ [a'', b'']
    where
        (a', a'') = operandCodeGen t a
        (b', b'') = operandCodeGen t b

operandCodeGen :: Map Label Word16 -> Operand -> (Word16, Maybe Word16)
operandCodeGen _ (Register SP) = (0x1b, Nothing)
operandCodeGen _ (Register PC) = (0x1c, Nothing)
operandCodeGen _ (Register O)  = (0x1d, Nothing)
operandCodeGen _ (Register r)  = (fromIntegral $ fromEnum r, Nothing)
operandCodeGen _ (Memory addr) = (0x1e, Just addr)
operandCodeGen _ (MemoryOffset addr r) = (fromIntegral (fromEnum r + 0x10), Just addr)
operandCodeGen _ (MemoryRegister r) = (fromIntegral (fromEnum r + 0x8), Nothing)
operandCodeGen _ Pop = (0x18, Nothing)
operandCodeGen _ Peek = (0x19, Nothing)
operandCodeGen _ Push = (0x1a, Nothing)
operandCodeGen t (Identifier l) = (0x1f, Just $ maybe 0 id (lookup l t))
operandCodeGen _ (Literal num) = if num < 32 then (num + 0x20, Nothing) else (0x1f, Just num)

firstPass stmts = fst $ last $ scanl firstPass' (empty, 0) stmts

firstPass' :: (Map Label Word16, Int) -> Statement -> (Map Label Word16, Int)
firstPass' (m, n) (Statement ls d) = (m', n')
    where
        m' = union (fromList $ zip ls $ repeat (fromIntegral n)) m
        n' = n + (length $ directiveCodeGen empty d)

secondPass stmts = concatMap (\(Statement _ d) -> directiveCodeGen m d) stmts
    where m = firstPass stmts

assemble s fileName = secondPass <$> parse program fileName s
assembleFile fileName = (fmap secondPass) <$> parseFromFile program fileName
