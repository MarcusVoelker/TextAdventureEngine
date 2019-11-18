module Renderer.Renderer where

import Logic.GameState
import Logic.Response

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

type View = GameState -> RendererState -> [String]

data Window = Window {
    _windowHandle :: WHandle,
    _windowLeft :: Int,
    _windowTop :: Int,
    _windowWidth :: Int,
    _windowHeight :: Int,
    _windowView :: View
}

data RendererState = RendererState {
    _rendererStateTextHistory :: [String],
    _rendererStateWindows :: M.Map WHandle Window
}

type Rendering a = StateT RendererState IO a

makeFields ''RendererState

makeFields ''Window

data RenderingException = RenderingException deriving (Show)

instance Exception RenderingException


renderWindow :: GameState -> Window -> Rendering ()
renderWindow gs win = do
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
        let contents = v gs rs
        forM_ (dropWhile ((<1).fst) $ zip [(y+h-1-length contents)..] contents) $ \(i,s) -> do
            setCursorPosition i 2
            putStr s

executeResponse :: GameState -> Response -> Rendering GameState
executeResponse gs (TextResponse s) = do
    textHistory %= (++lines s)
    return gs

executeResponses :: Responding GameState -> Rendering GameState
executeResponses (Responding responses gs) = foldM executeResponse gs responses

fallback :: SomeException -> IO (Maybe (Int,Int))
fallback = const $ return $ return $ (14,174)

safeGetTerminalSize :: IO (Maybe (Int,Int))
safeGetTerminalSize = catch getTerminalSize fallback

render :: GameState -> Rendering ()
render gs = do
    lift clearScreen
    wins <- use windows
    forM_ wins $ renderWindow gs
    size <- lift $ safeGetTerminalSize
    lift $ case size of
        Just (y,x) -> setCursorPosition (y-2) 2
        Nothing -> return ()
    lift $ putStr "> "
    lift $ hFlush stdout

initialRendererState :: IO RendererState
initialRendererState = do
    size <- safeGetTerminalSize
    case size of
        Just (y,x) -> return $ RendererState [] $ M.fromList [
            (1,Window 1 0 0 x (y-2) (\_ rs -> rs^.textHistory)),
            (0,Window 0 0 (y-3) x 3 (\_ _ -> [""]))]
        Nothing -> throw RenderingException

runRenderer :: Rendering () -> IO ()
runRenderer action = do 
    irs <- initialRendererState
    void $ execStateT action irs

renderEngine = Engine 
    clearScreen
    clearScreen