{-# LANGUAGE OverloadedStrings #-}

module Language.Whitespace.Assembly where

import Language.Whitespace.VM
import Data.Char
import Data.Void
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO
import Control.Monad
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.DeepSeq

decodeLabel :: Label -> String
decodeLabel l = decodeBinary
  where
    lId = labelId l
    decodeBinary = map decodeLabel' lId
    decodeLabel' b = if b then '1' else '0'

decodeArg :: Integer -> String
decodeArg num =
  if num >= 0x20 && num <= 0xff
    then show num ++ "; '" ++ (chr (fromInteger num) : "'")
    else show num

asmToken :: Instruction -> String
asmToken (Push arg)          = "    push " ++ decodeArg arg
asmToken Dup                 = "    dup"
asmToken (Ref arg)           = "    copy " ++ show arg
asmToken Shuffle             = "    shuffle"
asmToken (Slide arg)         = "    slide " ++ show arg
asmToken Swap                = "    swap"
asmToken Discard             = "    discard"

asmToken (Infix Plus)        = "    add"
asmToken (Infix Minus)       = "    sub"
asmToken (Infix Times)       = "    mul"
asmToken (Infix Divide)      = "    div"
asmToken (Infix Modulo)      = "    mod"

asmToken Retrieve            = "    load"
asmToken Store               = "    store"

asmToken (Label l)           = decodeLabel l ++ ":"
asmToken (Call l)            = "    call " ++ decodeLabel l
asmToken (Jump l)            = "    jmp " ++ decodeLabel l
asmToken (If Zero l)         = "    jz " ++ decodeLabel l
asmToken (If Negative l)     = "    js " ++ decodeLabel l
asmToken Return              = "    ret"
asmToken End                 = "    end"

asmToken ReadNum             = "    inn"
asmToken OutputNum           = "    outn"
asmToken ReadChar            = "    inc"
asmToken OutputChar          = "    outc"

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = L.signed sc (lexeme L.decimal)

asmParser :: Parser Program
asmParser = do
  result <- sepBy1 asmLine newline
  eof
  return $ catMaybes result

asmLine :: Parser (Maybe Instruction)
asmLine = do
  hspace
  stmt <- (asmStmt >>= return . Just) <|> (hspace >> return Nothing)
  return stmt


asmLabelId :: Parser Label
asmLabelId = do
  i <- many $ oneOf ['0', '1']
  return $ LabelId $ map (=='1') i

asmStmt :: Parser Instruction
asmStmt =
  (symbol "push" >> integer >>= return . Push)
  <|> (symbol "dup" >> return Dup)
  <|> (symbol "copy" >> integer >>= return . Ref . fromInteger)
  <|> (symbol "shuffle" >> return Shuffle)
  <|> (symbol "slide" >> integer >>= return . Slide . fromInteger)
  <|> (symbol "swap" >> return Swap)
  <|> (symbol "discard" >> return Discard)

  <|> (symbol "add" >> return (Infix Plus))
  <|> (symbol "sub" >> return (Infix Minus))
  <|> (symbol "mul" >> return (Infix Times))
  <|> (symbol "div" >> return (Infix Divide))
  <|> (symbol "mod" >> return (Infix Modulo))

  <|> (symbol "load" >> return Retrieve)
  <|> (symbol "store" >> return Store)

  <|> (symbol "call" >> asmLabelId >>= return . Call)
  <|> (symbol "jmp" >> asmLabelId >>= return . Jump)
  <|> (symbol "jz" >> asmLabelId >>= return . (If Zero))
  <|> (symbol "js" >> asmLabelId >>= return . (If Negative))
  <|> (symbol "ret" >> return Return)
  <|> (symbol "end" >> return End)

  <|> (symbol "inn" >> return ReadNum)
  <|> (symbol "inc" >> return ReadChar)
  <|> (symbol "outn" >> return OutputNum)
  <|> (symbol "outc" >> return OutputChar)

  <|> (asmLabelId >>= \l -> symbol ":" >>= \_ -> return (Label l))

hReadAsmFile :: FilePath -> Handle -> IO (Either (ParseErrorBundle Text Void) Program)
hReadAsmFile fname h = do
  input <- TIO.hGetContents h
  return $!! runParser asmParser fname input

readAsmFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Program)
readAsmFile fname = withFile fname ReadMode $ \h -> do
  hSetBuffering h LineBuffering
  hReadAsmFile fname h

hWriteAsmFile :: Handle -> Program -> IO ()
hWriteAsmFile h prg = forM_ prg $ \t -> hPutStrLn h $ asmToken t

writeAsmFile :: FilePath -> Program -> IO ()
writeAsmFile fname prg = withFile fname WriteMode $ \h -> do
  hSetBuffering h LineBuffering
  hWriteAsmFile h prg

catchParseError :: Either (ParseErrorBundle Text Void) Program -> IO Program
catchParseError (Right prg) = return prg
catchParseError (Left e) = putStr (errorBundlePretty e) >> fail "Parse error"

readAssemblyFile :: FilePath -> IO Program
readAssemblyFile fname = do
  source <- readAsmFile fname
  catchParseError source

hReadAssemblyFile :: FilePath -> Handle -> IO Program
hReadAssemblyFile fname h = do
  source <- hReadAsmFile fname h
  catchParseError source
