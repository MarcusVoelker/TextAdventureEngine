module MainLoop where

import Parser.ActionParser
import Parser.EntityParser
import Parser.ItemParser
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
    itemCode <- readFile "app/items.dat"
    parse (tokenizer >>> blocker >>> rooms) roomCode (\rMap ->
        parse (tokenizer >>> blocker >>> items) itemCode (\iMap ->
                parse (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) entityCode (\es -> do
                    let initial = initialState (fromJust $ M.lookup "init" rMap)  []
                    let initial' = execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial
                    initial' >>= mainLoop
                ) (\e -> putStrLn $ "While parsing entities: " ++ e)
            ) (\e -> putStrLn $ "While parsing items: " ++ e)
        ) (\e -> putStrLn $ "While parsing rooms: " ++ e)

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