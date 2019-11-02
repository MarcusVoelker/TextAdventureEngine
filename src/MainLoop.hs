module MainLoop where

import Parser.ActionParser
import Parser.RoomParser
import Parser.Tokenizer
import Logic.GameState
import Logic.DefaultActions
import Logic.Interaction
import Map.Room
import Sound.Engine
import Engine

import Text.LParse.Parser

import Control.Arrow
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

runGame :: IO ()
runGame = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    let rs = doParse (tokenizer >>> blocker >>> rooms) roomCode
    case rs of
        Left x -> print x
        Right rMap -> do
            let initial = initialState (fromJust $ M.lookup "init" rMap) []
            mainLoop initial

mainLoop :: GameState -> IO ()
mainLoop s = do
    putStr "> "
    hFlush stdout
    command <- getLine
    parse action command (\c -> do
        s' <- execStateT c s
        mainLoop s') (const $ do
            putStrLn "I did not understand that."
            mainLoop s
            )