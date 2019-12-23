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
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Maybe
import qualified Data.Map.Strict as M

fullParser :: (Renderer re) => String -> String -> String -> DCont r String (TAIO re StateStack)
fullParser r e i = do
    rMap <- fst <$> pFunc (tokenizer >>> blocker >>> rooms) r
    iMap <- fst <$> pFunc (tokenizer >>> blocker >>> items) i
    es <- fst <$> pFunc (tokenizer >>> blocker >>> Parser.EntityParser.entities rMap iMap) e
    let initial = initialState (fromJust $ M.lookup "init" rMap) []
    return $ executeResponses $ (\gs -> StateStack gs []) <$> execStateT (mapM_ (uncurry instantiateEntity) (mapMaybe (\(r,e) -> (,e) <$> r) es)) initial

runGame :: (Renderer r) => (StateStack -> TAIO r ()) -> IO ()
runGame ml = withEngine soundEngine $ do
    roomCode <- readFile "app/rooms.dat"
    entityCode <- readFile "app/entities.dat"
    itemCode <- readFile "app/items.dat"
    run (fullParser roomCode entityCode itemCode) (runFrontend (80,40).(>>= ml)) putStrLn

mainLoop :: (Renderer r) => StateStack -> TAIO r ()
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
        mainLoop ss'

mainOpenGL :: IO ()
mainOpenGL = do
    ifs <- initialFrontendState (80,40) :: IO (FrontendState OpenGLRenderer)
    playIO
        (InWindow "Hello World" (800,600) (500,200))
        black
        60
        ifs 
        renderFrontend
        (const return)
        (const return)

renderFrontend :: FrontendState -> IO Picture
renderFrontend fs = do
    let ws = M.elems (fs^.windows)
    return $ Pictures $ map (renderWindowGL (80,40)) ws

renderWindowGL :: (Renderer r) => (Int,Int) -> Window r -> Picture
renderWindowGL (cw,ch) win = 
    let x = fromIntegral $ win^.left in
    let y = fromIntegral $ win^.top in
    let w = fromIntegral $ win^.width in
    let h = fromIntegral $ win^.height in
    let hnd = win^.handle in
    Translate (-10*fromIntegral cw/2) (15*fromIntegral ch/2) $ Color (greyN (fromIntegral hnd)) $ Polygon [(x*10,-y*15),(10*(x+w),-y*15),(10*(x+w),-15*(y+h)),(x*10,-15*(y+h))]
    --Color green $ Polygon [(x*10,y*10),(x*10+w*10,y*10),(x*10+w*10,y*10+h*10),(x*10,y*10+h*10)]