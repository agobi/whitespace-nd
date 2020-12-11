module Language.Whitespace.Binary where

import Language.Whitespace.VM
import Language.Whitespace.Tokens
import System.IO
import Control.Monad
import Control.DeepSeq


{- Input to the whitespace VM.
   For convenience, three input characters
       A => space, B => tab, C => either of CR/LF

Numbers are binary (A=0, B=1, C=terminator)
Strings are sequences of binary characters, terminated by C.

We have:

* Stack instructions (Preceded by A)
     Push n        A
     Copy nth      BA
     Shuffle       BBA
     Slide n       BC
     Dup           CA
     Swap          CB
     Discard       CC

* Arithmetic (Preceded by BA)
     Plus          AA
     Minus         AB
     Times         AC
     Divide        BA
     Modulo        BB

* Heap access (Preceded by BB)
     Store         A
     Retrieve      B

* Control     (Preceded by C)
     Label String  AA
     Call Label    AB
     Jump Label    AC
     If Zero Label BA
     If Neg Label  BB
     Return        BC
     End           CC

* IO instructions (Preceded by BC)
     OutputChar    AA
     OutputNum     AB
     ReadChar      BA
     ReadNum       BB
     PushE         CC

-}

tokenise :: String -> [Token]
tokenise [] = []
tokenise (x:xs) | [x] == show A = A:(tokenise xs)
		| [x] == show B = B:(tokenise xs)
		| [x] == show C = C:(tokenise xs)
		| otherwise = tokenise xs

parse :: [Token] -> Program
parse [] = []
parse (A:A:xs) = let (num,rest) = parseNumber xs in
		  (Push num):(parse rest)
parse (A:C:A:xs) = Dup:(parse xs)
parse (A:B:A:xs) = let (num,rest) = parseNumber xs in
		   (Ref num):(parse rest)
parse (A:B:B:A:xs) = Shuffle:(parse xs)
parse (A:B:C:xs) = let (num,rest) = parseNumber xs in
		   (Slide num):(parse rest)
parse (A:C:B:xs) = Swap:(parse xs)
parse (A:C:C:xs) = Discard:(parse xs)

parse (B:A:A:A:xs) = (Infix Plus):(parse xs)
parse (B:A:A:B:xs) = (Infix Minus):(parse xs)
parse (B:A:A:C:xs) = (Infix Times):(parse xs)
parse (B:A:B:A:xs) = (Infix Divide):(parse xs)
parse (B:A:B:B:xs) = (Infix Modulo):(parse xs)

parse (B:B:A:xs) = Store:(parse xs)
parse (B:B:B:xs) = Retrieve:(parse xs)

parse (C:A:A:xs) = let (string,rest) = parseLabel xs in
		    (Label string):(parse rest)
parse (C:A:B:xs) = let (string,rest) = parseLabel xs in
		    (Call string):(parse rest)
parse (C:A:C:xs) = let (string,rest) = parseLabel xs in
		    (Jump string):(parse rest)
parse (C:B:A:xs) = let (string,rest) = parseLabel xs in
		    (If Zero string):(parse rest)
parse (C:B:B:xs) = let (string,rest) = parseLabel xs in
		    (If Negative string):(parse rest)

parse (C:B:C:xs) = Return:(parse xs)
parse (C:C:C:xs) = End:(parse xs)

parse (B:C:A:A:xs) = OutputChar:(parse xs)
parse (B:C:A:B:xs) = OutputNum:(parse xs)
parse (B:C:B:A:xs) = ReadChar:(parse xs)
parse (B:C:B:B:xs) = ReadNum:(parse xs)
parse (B:C:C:C:xs) = PushE:(parse xs)

parse _ = error "Unrecognised input"

unParse :: Instruction -> [Token] -> [Token]
unParse (Push arg)          c = A:A:unParseNumber arg c
unParse Dup                 c = A:C:A:c
unParse (Ref arg)           c = A:B:A:unParseNumber arg c
unParse Shuffle             c = A:B:B:A:c
unParse (Slide arg)         c = A:B:C:unParseNumber arg c
unParse Swap                c = A:C:B:c
unParse Discard             c = A:C:C:c

unParse (Infix Plus)        c = B:A:A:A:c
unParse (Infix Minus)       c = B:A:A:B:c
unParse (Infix Times)       c = B:A:A:C:c
unParse (Infix Divide)      c = B:A:B:A:c
unParse (Infix Modulo)      c = B:A:B:B:c

unParse Store               c = B:B:A:c
unParse Retrieve            c = B:B:B:c

unParse (Label label)       c = C:A:A:unParseLabel label c
unParse (Call label)        c = C:A:B:unParseLabel label c
unParse (Jump label)        c = C:A:C:unParseLabel label c
unParse (If Zero label)     c = C:B:A:unParseLabel label c
unParse (If Negative label) c = C:B:B:unParseLabel label c
unParse Return              c = C:B:C:c
unParse End                 c = C:C:C:c

unParse OutputChar          c = B:C:A:A:c
unParse OutputNum           c = B:C:A:B:c
unParse ReadChar            c = B:C:B:A:c
unParse ReadNum             c = B:C:B:B:c
unParse PushE               c = B:C:C:C:c


parseNumber :: Num x => [Token] -> (x, [Token])
parseNumber ts = parseNum' ts []
  where
    parseNum' (C:rest) acc = (makeNumber acc,rest)
    parseNum' (x:rest) acc = parseNum' rest (x:acc)

parseLabel :: [Token] -> (Label, [Token])
parseLabel ts = parseLabel' ts []
  where
    parseLabel' (C:rest) acc = (LabelId $ reverse acc,rest)
    parseLabel' (A:rest) acc = parseLabel' rest (False:acc)
    parseLabel' (B:rest) acc = parseLabel' rest (True:acc)

makeNumber :: Num x => [Token] -> x
makeNumber t
   | (last t) == A = makeNumber' (init t) 1
   | otherwise = -(makeNumber' (init t) 1)
  where
     makeNumber' [] _ = 0
     makeNumber' (A:rest) pow = (makeNumber' rest (pow*2))
     makeNumber' (B:rest) pow = pow + (makeNumber' rest (pow*2))


unParseNumber :: Integral a => a -> [Token] -> [Token]
unParseNumber i c =
  if i < 0
    then B : writeNumber (negate i) c
    else A : writeNumber i c
  where
    writeNumber n c = writeNumber' n [C] ++ c
    writeNumber' 0 acc = acc
    writeNumber' i acc =
      let (d, m) = i `divMod` 2
      in writeNumber' d $ (if m == 0 then A else B) : acc

unParseLabel :: Label -> [Token] -> [Token]
unParseLabel s = writeLabel' (labelId s)
  where
    writeLabel' []           c = C:c
    writeLabel' (False:rest) c = A:writeLabel' rest c
    writeLabel' (True:rest)  c = B:writeLabel' rest c

hWriteBinaryFile :: Handle -> Program -> IO ()
hWriteBinaryFile h prg = do
  forM_ (map show $ foldr unParse [] prg) (hPutStr h)

writeBinaryFile :: FilePath -> Program -> IO ()
writeBinaryFile fname prg =
  withFile fname WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    hWriteBinaryFile h prg

hReadBinaryFile :: Handle -> IO Program
hReadBinaryFile h = do
  prog <- hGetContents h
  let tokens = tokenise prog
  return $!! parse tokens

readBinaryFile :: FilePath -> IO Program
readBinaryFile fname = withFile fname ReadMode $ \h -> do
  hSetBuffering h LineBuffering
  hReadBinaryFile h
