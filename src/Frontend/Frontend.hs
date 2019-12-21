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

type View a = StateStack -> FrontendState a -> [String]

data Window a = Window {
    _windowHandle :: WHandle,
    _windowLeft :: Int,
    _windowTop :: Int,
    _windowWidth :: Int,
    _windowHeight :: Int,
    _windowView :: View a,
    _windowContext :: Int
}

data FrontendState a = FrontendState {
    _frontendStateTextHistory :: [String],
    _frontendStateWindows :: M.Map WHandle (Window a),
    _frontendStateRenderer :: a
}

type TAIO r a = StateT (FrontendState r) IO a

makeFields ''FrontendState

makeFields ''Window

data TAIOException = TAIOException deriving (Show)

instance Exception TAIOException

class Renderer a where
    initialRendererState :: IO a
    renderWindow :: StateStack -> Window a -> TAIO a ()
    dimensions :: TAIO a (Maybe (Int,Int))
    render :: StateStack -> TAIO a ()
    engine :: Engine

openTopWindow :: Int -> Int -> Int -> Int -> View r -> Int -> TAIO r ()
openTopWindow x y w h v c = do
    hand <- (+1) . maximum . M.keys <$> use windows
    let win = Window hand x y w h v c
    windows %= M.insert hand win

closeContextWindows :: Int -> TAIO r ()
closeContextWindows c = windows %= M.filter (\w -> w^.context /= c)

executeResponse :: (Renderer r) => StateStack -> Response -> TAIO r StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++lines s)
    return ss
executeResponse ss (InitiateDialogueResponse d) = do
    Just (y,x) <- dimensions
    openTopWindow 2 2 (x-4) (y-8) (\ss _ -> [head(ss^.stack)^.dialogue.response]) (contextCount ss + 1)
    return $ openContext (DialogueState d) ss
executeResponse ss LeaveContextResponse = do
    closeContextWindows $ contextCount ss
    return $ closeContext ss

executeResponses :: (Renderer r) => Responding StateStack -> TAIO r StateStack
executeResponses (Responding responses ss) = foldM executeResponse ss responses

initialFrontendState :: (Renderer r) => IO (FrontendState r)
initialFrontendState = do
    rend <- initialRendererState
    (size,rend') <- runStateT dimensions (FrontendState [] M.empty rend)
    case size of
        Just (y,x) -> return $ FrontendState [] (M.fromList [
            (0,Window 0 0 (y-3) x 3 (\_ _ -> [""]) 0),
            (1,Window 1 0 0 x (y-3) (\_ rs -> rs^.textHistory) 0)
            ]) (rend'^.renderer)
        Nothing -> throw TAIOException

runFrontend :: (Renderer r) => TAIO r () -> IO ()
runFrontend action = do 
    irs <- initialFrontendState
    void $ execStateT action irs