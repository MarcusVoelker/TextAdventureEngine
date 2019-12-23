module Frontend.Frontend where

import Logic.Dialogue
import Logic.GameState
import Logic.Response
import Logic.StateStack

import Engine

import Control.Exception
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import System.Console.ANSI
import System.IO (hFlush, stdout)

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

data FrontendState = FrontendState {
    _frontendStateTextHistory :: [String],
    _frontendStateWindows :: M.Map WHandle Window,
    _frontendStateDimensions :: (Int,Int)
}

type TAIO a = StateT FrontendState IO a

makeFields ''FrontendState

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
    (x,y) <- use dimensions
    openTopWindow 2 2 (x-4) (y-8) (\ss _ -> [head(ss^.stack)^.dialogue.response]) (contextCount ss + 1)
    return $ openContext (DialogueState d) ss
executeResponse ss LeaveContextResponse = do
    closeContextWindows $ contextCount ss
    return $ closeContext ss

executeResponses :: Responding StateStack -> TAIO StateStack
executeResponses (Responding responses ss) = foldM executeResponse ss responses

initialFrontendState :: (Int,Int) -> FrontendState
initialFrontendState (w,h) = FrontendState 
    [] 
    (M.fromList [
            (0,Window 0 0 (h-3) w 3 (\_ _ -> ["testtext"]) 0),
            (1,Window 1 0 0 w (h-3) (\_ rs -> rs^.textHistory) 0)
            ]) 
    (w,h)

runFrontend :: (Int,Int) -> TAIO () -> IO ()
runFrontend dims action = do 
    let irs = initialFrontendState dims
    void $ execStateT action irs