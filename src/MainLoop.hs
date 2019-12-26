module MainLoop where

import Parser.ActionParser
import Parser.ConfigParser
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

fullParser :: String -> String -> String -> String -> DCont r String (String,StateStack)
fullParser r e i c = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    cf <- fst <$> pFunc (tokenizer >>> blocker >>> config) c
    let initial = initialState (fromJust $ M.lookup "init" rMap) []
    return $ (cf,) $ (^.result) $ (\gs -> StateStack gs []) <$> execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: IO ()
runGame = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    configCode <- readFile "app/config.dat"
    run (fullParser roomCode entityCode itemCode configCode) (uncurry mainOpenGL) putStrLn

mainOpenGL :: String -> StateStack -> IO ()
mainOpenGL title ss = do
    let ifs = initialFrontendState (120,40) (8,16)
    playIO
        (InWindow title (120*8,40*16) (500,200))
        black
        60
        (ss,ifs)
        renderHandler
        eventHandler
        updateHandler

renderHandler :: (StateStack,FrontendState) -> IO Picture
renderHandler (ss,fs) = evalStateT (renderFrontend ss) fs

screenEffect :: TAIO Picture
screenEffect = do
    (cw,ch) <- uses (settings.dimensions) (bimap fromIntegral fromIntegral)
    (fw,fh) <- uses (settings.fontDimensions) (bimap fromIntegral fromIntegral)
    let w = fw*cw
    let h = fh*ch
    return $ Translate (fw*(-cw/2)) (fh*(ch/2)) $ 
        Pictures $ map (\y -> Color (makeColor 0 (fromIntegral (mod y 2)) 0 0.1) $ rect (0,-2*fromIntegral y) (w,2)) [0..div (round h-1) 2]

renderFrontend :: StateStack -> TAIO Picture
renderFrontend ss = do
    ws <- uses windows M.elems
    se <- screenEffect
    Pictures . (++[se]) <$> mapM (renderWindow ss) ws