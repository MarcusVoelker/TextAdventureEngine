module Renderer.Renderer where

import Logic.GameState
import Logic.Response

import Engine

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Console.ANSI
import System.IO (hFlush, stdout)

data RendererState = RendererState {
    _rendererStateTextHistory :: [String]
}

type Rendering a = StateT RendererState IO a

makeFields ''RendererState

data Window = Window {
    _windowLeft :: Int,
    _windowTop :: Int,
    _windowWidth :: Int,
    _windowHeight :: Int
}

makeFields ''Window

renderWindow :: Window -> IO ()
renderWindow win = do
    let x = win^.left
    let y = win^.top
    let w = win^.width
    let h = win^.height
    let bLine = "+" ++('-'<$[3..w])++ "+"
    let iLine = "|" ++(' '<$[3..w])++ "|"
    setCursorPosition y x
    putStr bLine
    forM_ [1..h-2] $ \o -> do
        setCursorPosition (y+o) x
        putStr iLine
    setCursorPosition (y+h-1) x
    putStr bLine

executeResponse :: GameState -> Response -> Rendering GameState
executeResponse gs (TextResponse s) = do
    textHistory %= (++lines s)
    return gs

executeResponses :: Responding GameState -> Rendering GameState
executeResponses (Responding responses gs) = foldM executeResponse gs responses

render :: GameState -> Rendering ()
render gs = do
    lift clearScreen
    size <- lift getTerminalSize
    th <- use textHistory
    lift $ case size of
        Just (y,x) -> do
            renderWindow (Window 0 0 x (y-2)) 
            forM_ (dropWhile ((<1).fst) $ zip [(y-3-length th)..] th) $ \(i,s) -> do
                setCursorPosition i 2
                putStr s
            renderWindow (Window 0 (y-3) x 3)
            setCursorPosition (y-2) 2
        Nothing -> putStrLn "Unsupported Terminal!"
    lift $ putStr "> "
    lift $ hFlush stdout

initialRendererState :: RendererState
initialRendererState = RendererState []

runRenderer :: Rendering () -> IO ()
runRenderer action = void $ execStateT action initialRendererState

renderEngine = Engine 
    clearScreen
    clearScreen