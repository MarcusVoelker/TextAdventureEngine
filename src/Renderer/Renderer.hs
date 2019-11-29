module Renderer.Renderer where

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

type View = StateStack -> RendererState -> [String]

data Window = Window {
    _windowHandle :: WHandle,
    _windowLeft :: Int,
    _windowTop :: Int,
    _windowWidth :: Int,
    _windowHeight :: Int,
    _windowView :: View,
    _windowContext :: Int
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

openTopWindow :: Int -> Int -> Int -> Int -> View -> Int -> Rendering ()
openTopWindow x y w h v c = do
    hand <- (+1) . maximum . M.keys <$> use windows
    let win = Window hand x y w h v c
    windows %= M.insert hand win

closeContextWindows :: Int -> Rendering ()
closeContextWindows c = windows %= M.filter (\w -> w^.context /= c)

renderWindow :: StateStack -> Window -> Rendering ()
renderWindow ss win = do
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

executeResponse :: StateStack -> Response -> Rendering StateStack
executeResponse ss (TextResponse s) = do
    textHistory %= (++lines s)
    return ss
executeResponse ss (InitiateDialogueResponse d) = do
    Just (y,x) <- lift safeGetTerminalSize
    openTopWindow 2 2 (x-4) (y-8) (\ss _ -> [head(ss^.stack)^.dialogue.response]) (contextCount ss + 1)
    return $ openContext (DialogueState d) ss
executeResponse ss LeaveContextResponse = do
    closeContextWindows $ contextCount ss
    return $ closeContext ss

executeResponses :: Responding StateStack -> Rendering StateStack
executeResponses (Responding responses ss) = foldM executeResponse ss responses

fallback :: SomeException -> IO (Maybe (Int,Int))
fallback = const $ return $ return (14,174)

safeGetTerminalSize :: IO (Maybe (Int,Int))
safeGetTerminalSize = catch getTerminalSize fallback

render :: StateStack -> Rendering ()
render ss = do
    lift clearScreen
    wins <- use windows
    forM_ wins $ renderWindow ss
    size <- lift safeGetTerminalSize
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
            (0,Window 0 0 (y-3) x 3 (\_ _ -> [""]) 0),
            (1,Window 1 0 0 x (y-3) (\_ rs -> rs^.textHistory) 0)
            ]
        Nothing -> throw RenderingException

runRenderer :: Rendering () -> IO ()
runRenderer action = do 
    irs <- initialRendererState
    void $ execStateT action irs

renderEngine = Engine 
    clearScreen
    clearScreen