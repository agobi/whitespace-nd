import Options.Applicative
import System.FilePath.Posix
import Language.Whitespace.Assembly (readAsmFile, writeAsmFile)
import Language.Whitespace.Binary (readBinaryFile, writeBinaryFile)


data MainArgs = MainArgs
  { disassemble :: Bool
  , inputFile :: FilePath
  , outputFile :: Maybe FilePath
  }
  deriving (Show)

mainArgs :: Parser MainArgs
mainArgs = MainArgs
  <$> switch (short 'd' <> help "disassemble")
  <*> strArgument (metavar "INPUT" <> help "input file")
  <*> optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT" <> help "output file"))

assembler :: FilePath -> FilePath -> IO ()
assembler inF outF = do
   Right prog <- readAsmFile inF
   writeBinaryFile outF prog


disassembler :: FilePath -> FilePath -> IO ()
disassembler inF outF = do
   prog <- readBinaryFile inF
   writeAsmFile outF prog


run :: MainArgs -> IO ()
run args = do
  if disassemble args
    then outF "wss" >>= disassembler inF
    else outF "ws" >>= assembler inF
  where
    inF = inputFile args
    outF :: String -> IO FilePath
    outF ext =
      case outputFile args of
        Just out -> return out
        Nothing  ->
          let out = replaceExtension inF ext
          in if out == inF
            then ioError $ userError "Input and output should be different"
            else return out

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (mainArgs <**> helper)
      ( fullDesc
     <> header "wsasm - whitespace assembler / disassembler" )
