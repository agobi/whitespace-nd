{-# LANGUAGE TemplateHaskell #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Language.Whitespace.VM
import Language.Whitespace.Input
import Language.Whitespace.Tokens
import Test.QuickCheck.Arbitrary.Generic
import System.Exit
import Debug.Trace 

import Test.QuickCheck.All

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

prop_reParse :: Instruction -> Bool
prop_reParse inst = [ inst ] == parsed
  where
    code = unParse inst []
    parsed = parse code

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  -- add test runners into the array for each module
  good <- and <$> sequence [runTests]
  if good
     then exitSuccess
     else exitFailure