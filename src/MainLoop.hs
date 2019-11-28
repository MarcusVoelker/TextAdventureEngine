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
import Logic.StateStack
import Map.Room
import Sound.Engine
import Engine
import Renderer.Renderer

import Text.LParse.Parser

import Control.Arrow
import Control.DoubleContinuations
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Maybe
import qualified Data.Map.Strict as M

fullParser :: String -> String -> String -> DCont r String (Rendering StateStack)
fullParser r e i = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    let initial = initialState (fromJust $ M.lookup "init" rMap) []
    return $ executeResponses $ (\gs -> StateStack gs []) <$> execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: IO ()
runGame = withEngine (soundEngine <> renderEngine) $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    run (fullParser roomCode entityCode itemCode) (runRenderer.(>>= mainLoop)) putStrLn

mainLoop :: StateStack -> Rendering ()
mainLoop ss = do
    render ss
    if noContext ss then do
        command <- lift getLine
        unless (command == "quit") $ do
            ss' <- parse action command 
                (\c -> executeResponses $ execStateT (liftBottom c) ss)
                (const $ executeResponse ss $ TextResponse "I did not understand that.")
            mainLoop ss'
    else
        undefined