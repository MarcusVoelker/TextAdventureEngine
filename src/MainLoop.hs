module MainLoop where

import Parser.ConfigParser
import Parser.EntityParser
import Parser.ItemParser
import Parser.RoomParser
import Parser.VariableParser
import Parser.Tokenizer
import Logic.GameState
import Logic.Interaction
import Logic.Response
import Logic.StateStack
import Sound.Engine
import Engine
import Frontend.Frontend

import Graphics.Gloss.Interface.IO.Game

import Text.LParse.Parser

import Control.Arrow hiding (left)
import Control.DoubleContinuations
import Control.Lens hiding (view)
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M

fullParser :: String -> String -> String -> String -> String -> DCont r String (String,StateStack)
fullParser r e i c v = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    (t,iR) <- fst <$> pFunc (tokenizer >>> blocker >>> config) c
    vs <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.VariableParser.variables) v
    let initial = initialState (fromJust $ M.lookup iR rMap) vs
    return $ (t,) $ (^.result) $ (\gs -> StateStack gs []) <$> execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: IO ()
runGame = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    configCode <- readFile "app/config.dat"
    variableCode <- readFile "app/variables.dat"
    run (fullParser roomCode entityCode itemCode configCode variableCode) (uncurry mainOpenGL) putStrLn

mainOpenGL :: String -> StateStack -> IO ()
mainOpenGL title ss = do
    let ifs = initialFrontendState (120,40) (8,16)
    playIO
        (InWindow title (120*8+1,40*16+1) (500,200))
        black
        60
        (ss,ifs)
        renderHandler
        eventHandler
        updateHandler