module Frontend.Frontend where

import Logic.Dialogue
import Logic.Driver
import Logic.GameState
import Logic.Response
import Logic.StateStack

import Engine

import Control.Exception
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char
import qualified Data.Map.Strict as M
import System.Exit
import System.IO (hFlush, stdout)

import Graphics.Gloss.Interface.IO.Game

type WHandle = Int

type View = StateStack -> FrontendState -> [String]

data Window = Window {
    _windowHandle :: WHandle,
    _windowLeft :: Int,
    _windowTop :: Int,
    _windowWidth :: Int,
    _windowHeight :: Int,
    _windowView :: View,
    _windowContext :: Int
}

data FrontendSettings = FrontendSettings {
    _frontendSettingsDimensions :: (Int,Int),
    _frontendSettingsFontDimensions :: (Int,Int)
}

data FrontendState = FrontendState {
    _frontendStateTextHistory :: [String],
    _frontendStateInputBuffer :: String,
    _frontendStateWindows :: M.Map WHandle Window,
    _frontendStateSettings :: FrontendSettings,
    _frontendStateElapsedTime :: Float
}

type TAIO a = StateT FrontendState IO a

makeFields ''FrontendState

makeFields ''FrontendSettings

makeFields ''Window

data TAIOException = TAIOException deriving (Show)

instance Exception TAIOException

openTopWindow :: Int -> Int -> Int -> Int -> View -> Int -> TAIO ()
openTopWindow x y w h v c = do
    hand <- (+1) . maximum . M.keys <$> use windows
    let win = Window hand x y w h v c
    windows %= M.insert hand win

closeContextWindows :: Int -> TAIO ()
closeContextWindows c = windows %= M.filter (\w -> w^.context /= c)

executeResponse :: StateStack -> Response -> TAIO StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++lines s)
    return ss
executeResponse ss (InitiateDialogueResponse d) = do
    (x,y) <- use (settings.dimensions)
    openTopWindow 2 2 (x-4) (y-8) (\ss _ -> [head(ss^.stack)^.dialogue.response]) (contextCount ss + 1)
    return $ openContext (DialogueState d) ss
executeResponse ss LeaveContextResponse = do
    closeContextWindows $ contextCount ss
    return $ closeContext ss
executeResponse ss QuitResponse = lift exitSuccess

executeResponses :: Responding StateStack -> TAIO StateStack
executeResponses (Responding responses ss) = foldM executeResponse ss responses

initialFrontendState :: (Int,Int) -> (Int,Int) -> FrontendState
initialFrontendState (w,h) (fw,fh) = FrontendState 
    [] []
    (M.fromList [
        (0,Window 0 0 (h-3) w 3 (\_ fs -> ["> " ++ (fs^.inputBuffer)]) 0),
        (1,Window 1 0 0 w (h-3) (\_ fs -> fs^.textHistory) 0)
        ]) 
    (FrontendSettings (w,h) (fw,fh))
    0

runFrontend :: (Int,Int) -> (Int,Int) -> TAIO () -> IO ()
runFrontend dims fdims action = do 
    let irs = initialFrontendState dims fdims
    void $ execStateT action irs

eventHandler :: Event -> (StateStack,FrontendState) -> IO (StateStack,FrontendState)
eventHandler e (ss,fs) = runStateT (handleEvent e ss) fs

handleEvent :: Event -> StateStack -> TAIO StateStack
handleEvent (EventKey (Char c) Down m _) ss 
    | ord c == 8 = do
        nn <- uses inputBuffer (not.null)
        when nn $
            inputBuffer %= init
        return ss
    | otherwise = do
        inputBuffer %= (++[if shift m == Down then toUpper c else c])
        return ss 
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) ss = do
    inputBuffer %= (++[' '])
    return ss
handleEvent (EventKey (SpecialKey KeyShiftR) Down _ _) ss = return ss
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) ss = do
    code <- use inputBuffer
    inputBuffer .= ""
    executeResponses $ executeCommand code ss
handleEvent (EventResize (x,y)) ss = do
    (fx,fy) <- use $ settings.fontDimensions
    let nx = div x fx
    let ny = div y fy
    (settings.dimensions) .= (nx,ny)
    return ss
handleEvent (EventKey _ Up _ _) ss = return ss
handleEvent (EventKey (MouseButton _) _ _ _) ss = return ss
handleEvent (EventMotion _) ss = return ss
handleEvent e ss = do
    lift $ putStrLn $ "Unhandled Event " ++ show e
    return ss

updateFrontend :: Float -> (StateStack,FrontendState) -> IO (StateStack,FrontendState)
updateFrontend t (ss,fs) = runStateT (stepFrontend t ss) fs

stepFrontend :: Float -> StateStack -> TAIO StateStack
stepFrontend t ss = do
    elapsedTime %= (+t)
    return ss