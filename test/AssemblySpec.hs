module AssemblySpec where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp

import Language.Whitespace.Assembly
import Language.Whitespace.Binary

import System.IO


testExample fname =
    [ testCase ("write / read assembly " ++ fname) $
        withSystemTempFile "test.wss" $ \tmpFile tmpHandle -> do
            prog <- readBinaryFile fname
            hWriteAsmFile tmpHandle prog
            hSeek tmpHandle AbsoluteSeek 0
            prog2 <- hReadAssemblyFile tmpFile tmpHandle
            assertEqual "Should reread assembly" prog prog2
    , testCase ("write / read binary " ++ fname) $
        withSystemTempFile "test.wss" $ \_ tmpHandle -> do
            prog <- readBinaryFile fname
            hWriteBinaryFile tmpHandle prog
            hSeek tmpHandle AbsoluteSeek 0
            prog2 <- hReadBinaryFile tmpHandle
            assertEqual "Should reread binary" prog prog2
    ]


test_assembly = testGroup "Examples"
    (  testExample "examples/calc.ws"
    ++ testExample "examples/count.ws"
    ++ testExample "examples/fact.ws"
    ++ testExample "examples/hanoi.ws"
    ++ testExample "examples/hworld.ws"
    ++ testExample "examples/loctest.ws"
    ++ testExample "examples/name.ws"
    ++ testExample "examples/randperm.ws"
    )
