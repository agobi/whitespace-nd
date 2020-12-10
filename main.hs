{-
**********************************************************************
     Whitespace - A language with no visible syntax.
     Copyright (C) 2003 Edwin Brady (e.c.brady@durham.ac.uk)

     This program is free software; you can redistribute it and/or
     modify it under the terms of the GNU General Public License
     as published by the Free Software Foundation; either version 2
     of the License, or (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License along
     with this program; if not, write to the Free Software Foundation, Inc.,
     59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

**********************************************************************
-}

module Main where

import Language.Whitespace.Binary (readBinaryFile)
import Language.Whitespace.Assembly (readAssemblyFile)
import Language.Whitespace.VM (execute)
import System.FilePath.Posix
import Options.Applicative


data MainArgs = MainArgs
  { wss :: Bool
  , inputFile :: FilePath
  }
  deriving (Show)

mainArgs :: Parser MainArgs
mainArgs = MainArgs
  <$> switch (short 's' <> long "source" <> help "input is whitespace source file")
  <*> strArgument (metavar "INPUT" <> help "input file")


run :: MainArgs -> IO ()
run args = do
  runtime <- (if source then readAssemblyFile else readBinaryFile) (inputFile args)
  execute runtime
  where
    source = wss args || takeExtension (inputFile args) == "wss"


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (mainArgs <**> helper)
      ( fullDesc <> header "wspace 0.4 (c) 2003 Edwin Brady" )
