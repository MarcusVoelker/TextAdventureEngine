module Frontend.ANSIRenderer where

import Engine
import Frontend.Frontend
import Logic.StateStack

import Control.Exception
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import System.Console.ANSI
import System.IO (hFlush, stdout)

data ANSIRenderer = ANSIRenderer

instance Renderer ANSIRenderer where
    initialRendererState = ansiInitialState
    renderWindow = ansiRenderWindow
    dimensions = ansiDimensions
    render = ansiRender
    engine = ansiEngine

ansiRenderWindow :: StateStack -> Window ANSIRenderer -> TAIO ANSIRenderer ()
ansiRenderWindow ss win = do
    let x = win^.left
    let y = win^.top
    let w = win^.width
    let h = win^.height
    let v = win^.view
    let bLine = "+" ++('-'<$[3..w])++ "+"
    let iLine = "|" ++(' '<$[3..w])++ "|"
    rs <- get
    lift $ do
        setCursorPosition y x
        putStr bLine
        forM_ [1..h-2] $ \o -> do
            setCursorPosition (y+o) x
            putStr iLine
        setCursorPosition (y+h-1) x
        putStr bLine
        let contents = v ss rs
        forM_ (dropWhile ((<1).fst) $ zip [(y+h-1-length contents)..] contents) $ \(i,s) -> do
            setCursorPosition i (x+2)
            putStr s

fallback :: SomeException -> IO (Maybe (Int,Int))
fallback = const $ return $ return (14,174)

ansiDimensions :: TAIO ANSIRenderer (Maybe (Int,Int))
ansiDimensions = lift $ catch getTerminalSize fallback

ansiInitialState :: IO ANSIRenderer
ansiInitialState = return ANSIRenderer

ansiRender :: StateStack -> TAIO ANSIRenderer ()
ansiRender ss = do
    lift clearScreen
    wins <- use windows
    forM_ wins $ renderWindow ss
    size <- dimensions
    lift $ case size of
        Just (y,x) -> setCursorPosition (y-3) 2
        Nothing -> return ()
    lift $ putStr "> "
    lift $ hFlush stdout

ansiEngine :: Engine
ansiEngine = Engine 
    clearScreen
    clearScreen