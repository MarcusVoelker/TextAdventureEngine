module MainLoop where

import Parser.ActionParser
import Parser.EntityParser
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
import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

runGame :: IO ()
runGame = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    let rs = doParse (tokenizer >>> blocker >>> rooms) roomCode
    case rs of
        Left x -> putStr "While parsing rooms: " >> print x
        Right rMap -> do
            let es = doParse (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap) entityCode
            case es of
                Left x -> putStr "While parsing entities: " >> print x
                Right es -> do
                    let initial = initialState (fromJust $ M.lookup "init" rMap) (mapMaybe (\(r,e) -> (,e) <$> r) es) []
                    mainLoop initial

mainLoop :: GameState -> IO ()
mainLoop s = do
    putStr "> "
    hFlush stdout
    command <- getLine
    unless (command == "quit") $
        parse action command (\c -> do
            s' <- execStateT c s
            mainLoop s') (const $ do
                putStrLn "I did not understand that."
                mainLoop s
                )