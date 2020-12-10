{-# LANGUAGE DeriveGeneric, DeriveAnyClass, NamedFieldPuns #-}

module Language.Whitespace.VM where

import GHC.Generics (Generic)
import Data.Array
import System.IO
import System.Random
import Control.DeepSeq

{- Stack machine for running whitespace programs -}

data Instruction
    = Push Integer
    | Dup
    | Ref Int
    | Shuffle
    | Slide Int
    | Swap
    | Discard
    | Infix Op
    | Store
    | Retrieve
    | Label Label
    | Call Label
    | Jump Label
    | If Test Label
    | Return
    | OutputChar
    | OutputNum
    | ReadChar
    | ReadNum
    | ErrNo
    | End
    deriving (Show, Eq, Generic, NFData)

data Op = Plus | Minus | Times | Divide | Modulo
   deriving (Show, Eq, Generic, NFData)

data Test = Zero | Negative
   deriving (Show, Eq, Generic, NFData)

newtype Label = LabelId { labelId :: [Bool] }
    deriving (Show, Eq, Generic, NFData)

type Loc = Integer

type Program = [Instruction]
newtype Stack = Stack [Integer]
type Heap = [Integer]
newtype Addr = Addr Integer

data VMState = VM
    { program :: Program
    , valstack :: Stack
    , callstack :: Stack
    , memory :: Heap
    , pcounter :: Int
    }

vm :: VMState -> IO ()
vm vmState@VM{program, pcounter} = do
    doInstr (vmState { pcounter = succ pcounter }) (program !! pcounter)


-- Running individual instructions
doInstr :: VMState -> Instruction -> IO ()
doInstr state@VM{valstack = Stack valstack} instr = case instr of
    Push n     | vs       <- valstack  -> withVStack (n:vs)
    Dup        | v:vs     <- valstack  -> withVStack (v:v:vs)
    Ref i      | vs       <- valstack  -> withVStack (vs!!i:vs)
    Shuffle    | vs       <- valstack  -> shuffle vs >>= withVStack
    Slide i    | v:vs     <- valstack  -> withVStack (v:drop i vs)
    Swap       | v1:v2:vs <- valstack  -> withVStack (v2:v1:vs)
    Discard    | _:vs     <- valstack  -> withVStack vs
    Infix op   | v1:v2:vs <- valstack  -> withVStack (doOp op v2 v1:vs)

    OutputChar | v:vs     <- valstack  -> writeChar v >> withVStack vs
    OutputNum  | v:vs     <- valstack  -> writeNum  v >> withVStack vs
    ReadChar   | v:vs     <- valstack  -> readChar >>= withVStackHeap vs . store (Addr v)
    ReadNum    | v:vs     <- valstack  -> readNum  >>= withVStackHeap vs . store (Addr v)

    Label _                            -> vm state
    End                                -> return ()
    Call l                             -> do
        loc <- findLabel l (program state)
        let Stack cs = callstack state
        let css = fromIntegral (pcounter state):cs
        vm $ state{callstack=Stack css, pcounter=loc}
    Jump l                             -> do
        loc <- findLabel l (program state)
        vm $ state{pcounter=loc}
    If t l     | v:vs     <- valstack  ->
        if test t v
            then do
                loc <- findLabel l (program state)
                vm $ state{pcounter=loc, valstack=Stack vs}
            else vm $ state{valstack=Stack vs}
    Return                             -> do
        let Stack(c:cs) = callstack state
        vm $ state{callstack=Stack cs, pcounter=fromInteger c}

    Store      | v:loc:vs <- valstack  -> withVStackHeap vs $ store (Addr loc) v
    Retrieve   | loc:vs   <- valstack  -> withVStack (retrieve (Addr loc) : vs)

    _                                  ->
         fail $ "Stack overflow execution instruction " ++ show instr

    where
        withVStack vs = vm $ state {valstack = Stack vs}
        withVStackHeap vs heap = vm $ state {valstack = Stack vs, memory=heap}

        -- Binary operators
        doOp Plus x y = x + y
        doOp Minus x y = x - y
        doOp Times x y = x * y
        doOp Divide x y = x `div` y
        doOp Modulo x y = x `mod` y

        -- Heap operations
        store :: Addr -> Integer -> Heap
        store (Addr addr) value = store' addr (memory state)
          where
            store' 0 (_:hs) = value : hs
            store' n (h:hs) = h : store' (n-1) hs
            store' 0 []     = [value]
            store' n []     = 0 : store' (n-1) []

        retrieve :: Addr -> Integer
        retrieve (Addr x) = memory state !! fromInteger x

        -- IO
        writeChar n = putChar (toEnum $ fromInteger n) >> hFlush stdout
        writeNum n = putStr (show n) >> hFlush stdout
        readChar = getChar >>= \ch -> return (toInteger $ fromEnum ch)
        readNum = getLine >>= \ch -> return (read ch :: Integer)

        -- Labels
        findLabel :: Label -> Program -> IO Int
        findLabel l p = case findLabel' p 0 of
            Just i  -> return i
            Nothing -> fail $ "Undefined label (" ++ show l ++ ")"
          where
            findLabel' :: Program -> Int -> Maybe Int
            findLabel' []            _           = Nothing
            findLabel' ((Label m):_) i | l == m  = Just i
            findLabel' (_:xs)        i           = findLabel' xs (i+1)

        test Negative = (< 0)
        test Zero     = (== 0)

        -- Shuffling the stack
        shuffle :: [Integer] -> IO [Integer]
        shuffle nums = do
            shuffled <- shuffle' arr 1
            return $ elems shuffled
            where
                arr = array (1, length nums) $ zip [1..] nums

                shuffle' :: Array Int Integer -> Int -> IO (Array Int Integer)
                shuffle' arr start = do
                    newIx <- randomRIO (start, end)
                    let (v1, v2) = (arr ! start, arr ! newIx)
                    -- putStrLn $ "Swapping " ++ (show (start, newIx))
                    let swapped = arr // [(start, v2), (newIx, v1)]
                    if start < (end - 1) then shuffle' swapped (start+1) else return arr
                    where
                        end = snd $ bounds arr


execute :: Program -> IO ()
execute program = do
  vm (VM program (Stack []) (Stack []) [] 0)
