{-# LANGUAGE FlexibleContexts #-}

module SerialiserTest where

import Serialiser

import Data.List
import Data.ByteString.Builder
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

data Testable = forall a . (Show a, Eq a, Persistent a ()) => MkTestable a

pack :: (Persistent a (), Show a, Eq a) => a -> Testable
pack = MkTestable

testStability :: Testable -> Bool
testStability (MkTestable a) = a == runReader (evalStateT deserialise (toLazyByteString (serialise a))) ()

tests :: [Testable]
tests = [pack (1 :: Int)
--    , pack 'c'
--    , pack "Party"
--    , pack (3 :: Int,"this is a test")
    , pack [3 :: Int, 4 :: Int, 5 :: Int]]
--    , pack (M.fromList [("a",1 :: Int),("b", 2 :: Int)])]

run :: IO ()
run = do
    let fails = filter (not . testStability) tests
    if null fails then
        putStrLn $ "Serialiser Test successful (" ++ show (length tests) ++ " cases tested)"
    else
        putStrLn $ "Serialiser Test(s) failed: [" ++ intercalate "," (map (\(MkTestable x) -> show x) fails) ++ "]"