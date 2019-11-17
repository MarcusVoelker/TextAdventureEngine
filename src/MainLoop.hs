module MainLoop where

import Parser.ActionParser
import Parser.EntityParser
import Parser.ItemParser
import Parser.RoomParser
import Parser.Tokenizer
import Logic.GameState
import Logic.DefaultActions
import Logic.Interaction
import Logic.Response
import Map.Room
import Sound.Engine
import Engine
import Renderer.Renderer

import Text.LParse.Parser

import Control.Arrow
import Control.DoubleContinuations
import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)

fullParser :: String -> String -> String -> DCont r String (IO GameState)
fullParser r e i = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    let initial = initialState (fromJust $ M.lookup "init" rMap) []
    return $ executeResponses $ execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: IO ()
runGame = withEngine (soundEngine <> renderEngine) $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    run (fullParser roomCode entityCode itemCode) (>>=mainLoop) putStrLn

mainLoop :: GameState -> IO ()
mainLoop s = do
    putStr "> "
    hFlush stdout
    command <- getLine
    unless (command == "quit") $
        parse action command (\c -> do
            s' <- executeResponses $ execStateT c s
            mainLoop s') (const $ do
                s' <- executeResponse s $ TextResponse "I did not understand that."
                mainLoop s'
                )