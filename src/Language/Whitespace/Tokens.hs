{-# LANGUAGE DeriveGeneric #-}

module Language.Whitespace.Tokens where

import GHC.Generics (Generic)

data Token = A | B | C 
  deriving (Eq, Generic)

instance Show Token where
    show A = " "
    show B = "\t"
    show C = "\n"
