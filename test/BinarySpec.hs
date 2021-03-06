{-# LANGUAGE TemplateHaskell #-}

module BinarySpec where

import Test.QuickCheck
import Language.Whitespace.VM
import Language.Whitespace.Binary
import Test.QuickCheck.Arbitrary.Generic
import System.Exit


instance Arbitrary Op where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Test where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Label where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Instruction where
  arbitrary = genericArbitrary
  shrink = genericShrink

prop_number :: Integer -> Bool
prop_number x = x == parsed && null rest
  where
    code = unParseNumber x []
    (parsed, rest) = parseNumber code

prop_label :: Label -> Bool
prop_label x = x == parsed && null rest
  where
    code = unParseLabel x []
    (parsed, rest) = parseLabel code

prop_instruction :: Instruction -> Bool
prop_instruction inst = [ inst ] == parsed
  where
    code = unParse inst []
    parsed = parse code

