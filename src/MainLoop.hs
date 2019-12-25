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
import Frontend.Frontend
import Frontend.Window

import Graphics.Gloss.Interface.IO.Game

import Text.LParse.Parser

import Control.Arrow hiding (left)
import Control.DoubleContinuations
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Maybe
import qualified Data.Map.Strict as M

fullParser :: String -> String -> String -> DCont r String StateStack
fullParser r e i = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    let initial = initialState (fromJust $ M.lookup "init" rMap) []
    return $ (^.result) $ (\gs -> StateStack gs []) <$> execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: IO ()
runGame = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    run (fullParser roomCode entityCode itemCode) mainOpenGL putStrLn

mainOpenGL :: StateStack -> IO ()
mainOpenGL ss = do
    let ifs = initialFrontendState (120,40) (8,16)
    playIO
        (InWindow "Hello World" (120*8,40*16) (500,200))
        black
        60
        (ss,ifs)
        renderFrontend
        eventHandler
        updateFrontend

renderFrontend :: (StateStack,FrontendState) -> IO Picture
renderFrontend (ss,fs) = do
    let dims = fs^.settings.dimensions
    let fdims = fs^.settings.fontDimensions
    let ws = M.elems (fs^.windows)
    return $ Pictures $ map (renderWindow (ss,fs)) ws