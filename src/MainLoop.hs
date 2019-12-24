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

{-mainLoop :: StateStack -> TAIO ()
mainLoop ss = do
    render ss
    if noContext ss then do
        command <- getInput
        unless (command == "quit") $ do
            ss' <- parse action command 
                (\c -> executeResponses $ execStateT (liftBottom c) ss)
                (const $ executeResponse ss $ TextResponse "I did not understand that.")
            mainLoop ss'
    else do
        command <- lift getLine
        ss' <- executeResponses $ execStateT (liftTemporary (tempAction command)) ss
        mainLoop ss'-}

mainOpenGL :: StateStack -> IO ()
mainOpenGL ss = do
    let ifs = initialFrontendState (120,40) (6,13)
    playIO
        (InWindow "Hello World" (120*6,40*13) (500,200))
        black
        60
        (ss,ifs)
        renderFrontend
        (const return)
        (const return)

renderFrontend :: (StateStack,FrontendState) -> IO Picture
renderFrontend (ss,fs) = do
    let dims = fs^.settings.dimensions
    let fdims = fs^.settings.fontDimensions
    let ws = M.elems (fs^.windows)
    return $ Pictures $ map (renderWindowGL (ss,fs)) ws

renderWindowGL :: (StateStack,FrontendState) -> Window -> Picture
renderWindowGL (ss,fs) win = 
    let (cw,ch) = bimap fromIntegral fromIntegral $ fs^.settings.dimensions in
    let (fw,fh) = bimap fromIntegral fromIntegral $ fs^.settings.fontDimensions in
    let x = fromIntegral $ win^.left in
    let y = fromIntegral $ win^.top in
    let w = fromIntegral $ win^.width in
    let h = fromIntegral $ win^.height in
    let v = win^.view in
    let hnd = fromIntegral $ win^.handle in
    Translate (fw*(x-cw/2)) (fh*(-y+ch/2)) $ Pictures (Color (greyN hnd) (Polygon [(0,0),(fw*w,0),(fw*w,-fh*h),(0,-fh*h)]):map (Translate 10 (-10) . Color green . Scale 0.1 0.1 . Text) ["test"])